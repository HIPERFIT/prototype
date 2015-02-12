-- NOTE:
-- Have to copy a lot of code from process library just
-- to add working directory argument to readProcess.
-- This is ridiculous, but it seems that there is no other way.

{-# LANGUAGE CPP #-}
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE InterruptibleFFI #-}
#endif
module ProcessUtils (readProcessWorkDir) where

import Prelude hiding (mapM)
import System.Process
#ifndef __HUGS__
import System.Process.Internals

import Control.Exception (SomeException, mask, try, throwIO)
import Control.DeepSeq (rnf)
import System.IO.Error (mkIOError, ioeSetErrorString)
#if !defined(mingw32_HOST_OS)
import System.Posix.Types
import System.Posix.Process (getProcessGroupIDOf)
#endif
import qualified Control.Exception as C
import Control.Concurrent
import Control.Monad
import Foreign
import Foreign.C
import System.IO
import Data.Maybe
#endif
import System.Exit      ( ExitCode(..) )

#ifdef __GLASGOW_HASKELL__
import GHC.IO.Exception ( ioException, IOErrorType(..), IOException(..) )
#if defined(mingw32_HOST_OS)
import System.Win32.Process (getProcessId)
import System.Win32.Console (generateConsoleCtrlEvent, cTRL_BREAK_EVENT)
#else
import System.Posix.Signals
#endif
#endif

#ifdef __HUGS__
import Hugs.System
#endif

-- added wd (working directory) argument to original readProcess
readProcessWorkDir wd cmd args input = do
  let cp_opts = (proc cmd args) {
                    std_in  = CreatePipe,
                    std_out = CreatePipe,
                    std_err = CreatePipe,  -- changed form Inherit to suppress output
                    cwd = Just wd
                  }
  (ex, output) <- withCreateProcess_ "readProcess" cp_opts $
                  \(Just inh) (Just outh) _ ph -> 
                      do
                        
                        -- fork off a thread to start consuming the output
                        output  <- hGetContents outh
                        withForkWait (C.evaluate $ rnf output) $ \waitOut -> 
                            do
                              -- now write any input
                              unless (null input) $
                                     ignoreSigPipe $ hPutStr inh input
                              -- hClose performs implicit hFlush, and thus may trigger a SIGPIPE
                              ignoreSigPipe $ hClose inh

                              -- wait on the output
                              waitOut
                              hClose outh

                        -- wait on the process
                        ex <- waitForProcess ph
                        return (ex, output)

  case ex of
    ExitSuccess   -> return output
    ExitFailure r -> processFailedException "readProcess" cmd args r


withCreateProcess_
  :: String
  -> CreateProcess
  -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a)
  -> IO a
withCreateProcess_ fun c action =
    C.bracketOnError (createProcess_ fun c) cleanupProcess
                     (\(m_in, m_out, m_err, ph) -> action m_in m_out m_err ph)


cleanupProcess :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
               -> IO ()
cleanupProcess (mb_stdin, mb_stdout, mb_stderr, ph) = do
    terminateProcess ph
    -- Note, it's important that other threads that might be reading/writing
    -- these handles also get killed off, since otherwise they might be holding
    -- the handle lock and prevent us from closing, leading to deadlock.
    maybe (return ()) (ignoreSigPipe . hClose) mb_stdin
    maybe (return ()) hClose mb_stdout
    maybe (return ()) hClose mb_stderr
    -- terminateProcess does not guarantee that it terminates the process.
    -- Indeed on Unix it's SIGTERM, which asks nicely but does not guarantee
    -- that it stops. If it doesn't stop, we don't want to hang, so we wait
    -- asynchronously using forkIO.
    _ <- forkIO (waitForProcess ph >> return ())
    return ()

withForkWait :: IO () -> (IO () ->  IO a) -> IO a
withForkWait async body = do
  waitVar <- newEmptyMVar :: IO (MVar (Either SomeException ()))
  mask $ \restore -> do
    tid <- forkIO $ try (restore async) >>= putMVar waitVar
    let wait = takeMVar waitVar >>= either throwIO return
    restore (body wait) `C.onException` killThread tid

ignoreSigPipe :: IO () -> IO ()
#if defined(__GLASGOW_HASKELL__)
ignoreSigPipe = C.handle $ \e -> case e of
                                   IOError { ioe_type  = ResourceVanished
                                           , ioe_errno = Just ioe }
                                     | Errno ioe == ePIPE -> return ()
                                   _ -> throwIO e
#else
ignoreSigPipe = id
#endif


processFailedException :: String -> String -> [String] -> Int -> IO a
processFailedException fun cmd args exit_code =
      ioError (mkIOError OtherError (fun ++ ": " ++ cmd ++
                                     concatMap ((' ':) . show) args ++
                                     " (exit " ++ show exit_code ++ ")")
                                 Nothing Nothing)
