module ReadToken where

import HsSexp

-- TODO: HaskellPlatformを知らずparsecがないことが前提だったが、parsecがあるようなので使いたい。または、最新のパーザを使う.

isHex x = elem x "1234567890abcdeABCDE"
isNumber x = elem x "1234567890"
isAlpha x = elem x "abcdefghijklmnopqrsturvwxyzABCDEFGHIJKLMNOPQRSTURVWXYZ0123456789_"
isSymbol x = elem x "!#$%&-=^~|*:@+<>./?"
isSpecialAlpha x = x == '\''
isWhite x = elem x " \t"

readExpr :: String -> String
readExpr input = case parse symbol "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

            
readToken = readNumber <&&&> 

-- 行単位で処理する場合Comment判定が単独ではできないため、ParseErrorは無い
            
-- TODO: UTF8対応? let あ = 1 in あ はコンパイルエラー
--  CPPの中のlong commmentはどうなる？
readToken :: [Token] -> String -> [Token]
readToken knil [] = reverse knil
readToken knil (x:xs)
    | isNumber x = readTokenNumber knil (x:"") xs
    | isWhite x = readTokenWhite knil (x:"") xs
    | isAlpha x = readTokenAlpha knil (x:"") xs
    | isSymbol x = readTokenSymbol knil (x:"") xs
    | x == '{' = readTokenBrace knil xs
    | elem x "()[]}" = readToken (Paren x:knil) xs
    | x == '\\' =readTokenBackSlash knil xs
    | x == '"' = readTokenDouble knil "\"" xs
    | x == '\'' = readTokenChar knil "'"xs
    | x == '`' = readBackQuote knil "`" xs
    | x == ';' = readToken (SemiColon : knil) xs
    | x == '#' = readTokenCPP knil xs
    | x == ',' = readToken (Comma:knil) xs
    | otherwise = readToken (HsSexpError x :knil) xs

readTokenCPP knil line = reverse (Cpp line: knil)

readTokenHex knil w [] = reverse (Alpha "x": Num (reverse w):knil)
readTokenHex knil w (x:xs)
    | isHex x = readTokenHex2 knil (x:'x':w) xs
    | otherwise = readToken (Num (reverse w):knil) (x:'x':xs)

readTokenHex2 knil w [] = reverse (Num (reverse w):knil)
readTokenHex2 knil w (x:xs)
    | isHex x = readTokenHex2 knil (x:w) xs
    | otherwise = readToken (Num (reverse w):knil) (x:xs)

-- TODO: "-                2.0e-2" を一つの数字として扱うかどうか。
readTokenNumber knil w [] = reverse (Num w:knil)
readTokenNumber knil w (x:xs)
    | isNumber x = readTokenNumber2 knil (x:w) xs
    | x == 'e' = readTokenNumber5 knil w xs
    | x == '.' = readTokenNumber3 knil w xs
    | head w == '0' && x == 'x' = readTokenHex knil w xs
    | otherwise = readToken knil (x:xs)

readTokenNumber2 knil w [] = reverse (Num (reverse w):knil)
readTokenNumber2 knil w (x:xs)
    | isNumber x = readTokenNumber2 knil (x:w) xs
    | x == 'e' = readTokenNumber5 knil w xs
    | x == '.' = readTokenNumber3 knil w xs
    | otherwise = readToken (Num (reverse w):knil) (x:xs)

readTokenNumber3 knil w [] = reverse (Symbol ".":Num (reverse w):knil)
readTokenNumber3 knil w (x:xs)
    | isNumber x = readTokenNumber4 knil (x:w) xs
    | otherwise = readToken (Symbol ".":Num (reverse w):knil) xs

readTokenNumber4 knil w [] = reverse (Num (reverse w):knil)
readTokenNumber4 knil w (x:xs)
    | isNumber x = readTokenNumber4 knil (x:w) xs
    | x ==  'e' = readTokenNumber5 knil w xs
    | otherwise = readToken (Num (reverse w):knil) (x:xs)

readTokenNumber5 knil w [] = reverse (Alpha "e":Num (reverse w):knil)
readTokenNumber5 knil w (x:xs)
    | x == '-' = readTokenNumber6 knil w xs
    | isNumber x = readTokenNumber7 knil (x:'e':w) xs
    | otherwise = readToken (Num (reverse w):knil) (x:'e':xs)

readTokenNumber6 knil w [] = reverse (Symbol "-":Alpha "e":Num (reverse w):knil)
readTokenNumber6 knil w (x:xs)
    | isNumber x = readTokenNumber7 knil (x:'-':'e':w) xs
    | otherwise = readToken (Alpha "e":Num (reverse w):knil) (x:'-':xs)

readTokenNumber7 knil w [] = reverse (Num (reverse w):knil)
readTokenNumber7 knil w (x:xs)
    | isNumber x = readTokenNumber7 knil (x:w) xs
    | otherwise = readToken (Num (reverse w):knil) (x:xs)

readTokenBackSlash knil [] = reverse (Func : knil)
readTokenBackSlash knil (x:xs)
    | x == '\\' = readTokenSymbol knil "\\\\" xs
    | otherwise = readToken (Func:knil) (x:xs)

readTokenBrace knil [] = reverse (Paren '{' : knil)
readTokenBrace knil (x:xs)
    | (x == '-')  = readToken (CommentLong :knil) xs
    | otherwise = readToken (Paren '{':knil) (x:xs)

readTokenAlpha knil w [] = reverse (Alpha (reverse w):knil)
readTokenAlpha knil w (x:xs)
    | isAlpha x  || isSpecialAlpha x= readTokenAlpha knil (x:w) xs
    | otherwise = readToken (Alpha (reverse w):knil) (x:xs)

readTokenSymbol knil w [] = reverse (Symbol (reverse w):knil)
readTokenSymbol knil w (x:xs)
    | isSymbol x = readTokenSymbol knil (x:w) xs
    | x == '\\' = readTokenSymbol2 knil w xs
    | x == '}' && head w == '-' = readTokenSymbol ((reverse knil)  xs --0文字のSymbolを許すかどうか。
                                  where knil = if (tail w == "")
                                               then Symbol ("}-") :knil
                                               else tail w: knil
    | otherwise = readToken (Symbol (reverse w):knil) (x:xs)

readTokenSymbol2 knil w [] = reverse (Symbol (reverse w) :knil)
readTokenSymbol2 knil w (x:xs)
    | x == '\\' = readTokenSymbol knil ('\\':'\\':w) xs
    | otherwise = readToken (Func : (Symbol $reverse w):knil) (x:xs)

readTokenWhite knil w [] = reverse (White (reverse w):knil)
readTokenWhite knil w (x:xs)
    | isWhite x = readTokenWhite knil (x:w) xs
    | otherwise = readToken (White (reverse w):knil) (x:xs)

readTokenDouble knil w [] = reverse (Str (reverse w):knil)
readTokenDouble knil w (x:xs)
    | x == '"' =  readToken ((Str $ reverse ('"':w)): knil) xs
    | x == '\\' =
        if (xs == "")
        then reverse ((Str $ reverse ('\\':w)):knil)
        else readTokenDouble knil (head xs:'\\':w) $ tail xs
    | otherwise = readTokenDouble knil (x:w) xs

readTokenChar knil w [] = reverse (Char (reverse w):knil)
readTokenChar knil w (x:xs)
    | x == '\'' =  readToken ((Char $ reverse ('\'':w)): knil) xs
    | x == '\\' =
        if (xs == "")
        then reverse ((Char $ reverse ('\\':w)):knil)
        else readTokenChar knil (head xs:'\\':w) $ tail xs
    | otherwise = readTokenChar knil (x:w) xs

readBackQuote knil w (x:xs)
    | x == '`' = readToken (BackQuote (reverse w):knil) xs
    | isAlpha x || isWhite x || isSpecialAlpha x = readBackQuote knil (x:w) xs
    | otherwise = panic "reading char"
