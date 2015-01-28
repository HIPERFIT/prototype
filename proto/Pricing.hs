module Pricing (runPricing) where

import CodeGen.OpenclGen
import CodeGen.Utils
import CodeGen.DataGen
import qualified Config as Conf
import Contract
import ProcessUtils

import System.Directory
import System.Process

payoffSource = "MediumContract.cl"
refPrices = "\n[0.0]" -- fake reference prices

runPricing :: [(DiscModel, ModelData, MarketData)] -> MContract -> IO [Double]
runPricing ds mContr = 
    do
      inputData <- generateData ds mContr
      writeOpenCL (ppCLSeq $ genPayoffFunc $ fromManaged mContr) payoffSource
      copyFile (Conf.genCodePath ++ payoffSource) (Conf.pricerCodePath ++ payoffSource)
      recompileBenchmark
      a <- readProcessWorkDir Conf.pricingEnginePath "./GenPricing" [] $ inputData ++ refPrices
      return $ parseOut a      

-- TODO: looks like full recompilation not needed.
recompileBenchmark = do
  readProcess "make" ["-C",  Conf.pricingEnginePath, "clean", "gpu"] []

-- Alternative implementation where all input (including payoff function code)
-- is provided in stdin. Works only with special support from finpar workbench.
-- Left as experimental implementation.
_runPricingAlt ds mContr = do 
  inputData <- generateData ds mContr
  putStrLn inputData
  kernelSource <- 
      do 
        template <- readFile "proto/templ/KernelTemplate.cl"
        return $ replaceLabel "CODE" (ppCLSeq $ genPayoffFunc $ fromManaged mContr) template
  putStrLn kernelSource
  return $ readProcessWorkDir Conf.pricingEnginePath "./GenPricing" [] (kernelSource ++ inputData ++ " [ 0.0 ]\n")
