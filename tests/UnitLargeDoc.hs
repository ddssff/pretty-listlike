module UnitLargeDoc where

import Text.PrettyPrint.HughesPJ

import Control.DeepSeq
import Control.Exception
import Data.Time (diffUTCTime, getCurrentTime, NominalDiffTime)

testLargeDoc :: IO ()
testLargeDoc = do
  putStrLn "Testing large doc..."
  (r, t) <- timeTask $ evaluate largeDocRender
  putStrLn $ "Elapsed: " ++ show t
  return ()

largeDocRender :: String
largeDocRender = force $ render $ vcat $ replicate 10000000 $ text "Hello"

-- | Run a task and return the elapsed time along with its result.
timeTask :: IO a -> IO (a, NominalDiffTime)
timeTask x =
    do start <- getCurrentTime
       result <- x >>= evaluate
       finish <- getCurrentTime
       return (result, diffUTCTime finish start)
