module Main where

import           ReadBType (readInput)

main :: IO ()
main = do
  print $ readInput "89"
  print $ readInput "asdasdasd"
  print $ readInput "[1,2,3,4,5]"
  print $ readInput "[1,2,-4,-5,sdfsdf,sddfdf]"
  print $ readInput "[1,2,-4,-5,abc,asdag, [1,2,3], [asda,gaga]]"
  print $ readInput "[\"nancy\", \"germany\", 56, 53, -3, [], []]"