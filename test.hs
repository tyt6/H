module Main where

import GHC.IOArray

test | 1 == 1 =
 if (2== 2)
                               then 2
                                                          else if (True)
 then 1
                                                                             else
 2
                                                   | otherwise = 3

test2222 y = '"'
plus' x y = map (\ (x,y) -> x + y) $ zip x y
test2333 = [1] ` 	plus'` [2]
test3333 =  True || False

-------------------------------------

-- test2 = 'aa'

-- {-
testInComment = ""
-- -}
        
-- testx = "
--         a"

main = do
  ioarr1 <- newIOArray (0, 10) 0
  ioarr2 <- newIOArray (0, 10) ioarr1
  writeIOArray ioarr2 0 ioarr1
  ioarr1a <- readIOArray ioarr2 0
  x <- readIOArray ioarr1a 0
  putStr $ show x
  return x
