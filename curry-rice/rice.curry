import System.Environment (getProgName)

isCurryRiceProgram :: String -> Bool
isCurryRiceProgram code = contains "Curry" code && contains "Rice" code

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

swapCurryRice :: String -> String
swapCurryRice [] = []
swapCurryRice s@(x:xs)
  | prefix "Rice" s = "Curry" ++ swapCurryRice (drop (length "Rice") s)
  | prefix "Curry"  s = "Rice" ++ swapCurryRice (drop (length "Curry") s)
  | otherwise        = [x] ++ swapCurryRice xs

main :: IO ()
main = do
  progName <- getProgName
  let filename = progName ++ ".curry"
  content <- readFile filename
  if isCurryRiceProgram content
    then putStr (swapCurryRice content)
    else putStrLn "Not a Curry-Rice program."
