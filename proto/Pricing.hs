module Pricing where

import CodeGen.OpenclGen
import CodeGen.Utils
import CodeGen.DataGen
import qualified Config as Conf
import Contract

import System.Directory
import System.Process

runPricing :: [(DiscModel, ModelData, MarketData)] -> MContract -> IO [Double]
runPricing ds mContr = 
    do
      genAndWriteData ds  mContr
      writeOpenCL (ppCLSeq $ genPayoffFunc $ fromManaged mContr) "MediumContract"
      copyDataAndCode "input.data" "MediumContract.cl"
      recompileBenchmark
      a <- readProcess "make" ["-C", Conf.pricingEnginePath, "run_medium"] []
      return $ parseOut a

copyDataAndCode dataFile sourceFile = 
    do
      copyFile (Conf.genDataPath ++ dataFile) (Conf.pricerDataPath ++ dataFile)
      copyFile (Conf.genCodePath ++ sourceFile) (Conf.pricerCodePath ++ sourceFile)

recompileBenchmark = do
  res <- readProcess "make"  ["-C",  Conf.pricingEnginePath, "clean", "gpu"] []
  putStrLn res
