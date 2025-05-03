import System.Environment (getProgName)

isRiceCurryProgram :: String -> Bool
isRiceCurryProgram code = contains "Rice" code && contains "Curry" code

contains :: String -> String -> Bool
contains [] _ = True
contains _ [] = False
contains sub str@(_:xs)
  | prefix sub str = True
  | otherwise      = contains sub xs

prefix :: String -> String -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (x:xs) (y:ys) = x == y && prefix xs ys

swapRiceCurry :: String -> String
swapRiceCurry [] = []
swapRiceCurry s@(x:xs)
  | prefix "Curry" s = "Rice" ++ swapRiceCurry (drop (length "Curry") s)
  | prefix "Rice"  s = "Curry" ++ swapRiceCurry (drop (length "Rice") s)
  | otherwise        = [x] ++ swapRiceCurry xs

main :: IO ()
main = do
  progName <- getProgName
  let filename = progName ++ ".curry"
  content <- readFile filename
  if isRiceCurryProgram content
    then putStr (swapRiceCurry content)
    else putStrLn "Not a Rice-Curry program."
