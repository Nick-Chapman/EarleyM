-- Bare bones testing framework. TODO: use a real testing framework!
module Testing (runAll,check) where
       
import Control.Exception
import System.Exit

check :: (Show a, Show b, Eq b) => (a -> b) -> String -> a -> b -> IO Bool
check f tag arg expect =
  let actual = f arg in
  if (actual == expect)
  then do putStr ("PASS: " ++ tag ++ ": " ++ show arg ++ "\n"); return True
  else do putStr ("FAIL: " ++ tag ++ ": " ++ show arg ++
                  "\n--Expect = " ++ show expect ++
                  "\n--Actual = " ++ show actual ++ "\n"); return False

runAll :: [IO Bool] -> IO ()
runAll xs =  do
  results <- sequence (map wrap xs)
  let isPass r = case r of Just True -> True ; _ -> False
  let isFail r = case r of Just False -> True ; _ -> False
  let isException r = case r of Nothing -> True ; _ -> False
  let p = length (filter isPass results)
  let f = length (filter isFail results)
  let e = length (filter isException results)
  putStr (
    "#pass = " ++ show p ++ ", " ++
    "#fail = " ++ show f ++ ", " ++
    "#exception = " ++ show e ++ "\n")
  if (f+e > 0) then exitFailure else exitSuccess

wrap :: IO a -> IO (Maybe a)
wrap io = handle (\e -> do
  putStr ("EXCEPTION: " ++ show (e::SomeException) ++ "\n")
  return Nothing) (fmap Just io)
