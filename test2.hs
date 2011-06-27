 module Main where
--  制御文字いれてはいけない。
import Data.Maybe
{-
 
{-
test
-- -}
test
-- -}
{----}
x {-!-} y = y -- emacs haskell-mode (v2_4) は正しく判定しないようだ
-- x !-} y = x + y -- !- }
{--}
yyyyyy = {-
---}'t' -- 'test'
--guard
testg (x:xs)
 | True = 1 + n
 where n = length xs

-- indentはtopレベル(新しい関数)で判定される。
test = putStr $ (testInComment++)$ show $  (1 +) $ (\
 x-> x*2) 2 --1つのインデントが必要なため繋がっているわけではない
 where length::Integer -> [Maybe Bool]  ->  Integer
-- indent2 where句は揃わなくてはいけない。コメントはどうでもいい。
       length n (x:xs) = case x of
        --新しい関数で1つindentしなければいけない。
        _-> 1
        +
        nn + n
--        with nn = 0
        where nn =0
--	   length n [] = n -- tab は駄目
--      	length n [] = n  -- tabはindentではない
       x	++++ y = y	x -- tabはindent以外ではスペースの代わりになる(y[tab]x)
       y a = 1 ++++ (\x -> x + 1)
-- {-
testInComment = ""
-- -}
--test221 = 0x
--testxxx = .0 --  関数合成と 0？
--testx2q = 0. -- 関数合成 と 0？
--testx21
test222 = 0.2
test223 = -                    0.2
test224 = 0x12aA
test225 = -                         1.0e-2
--test226 = -                         1.e-2
-- test224 = 0.
-- x , y = x + y

main:: IO ()
main = do
  -- x <- 1
  putStr $ show $ test225* 100
