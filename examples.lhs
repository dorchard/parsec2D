> import Text.ParserCombinators.Parsec
> import Text.ParserCombinators.Parsec.TwoD

> spaces' = many (char ' ')

> spacedChar = do
>  spaces'
>  n <- anyChar
>  spaces'
>  return n

Parsers a pair of characters where one is vertically above the other
e.g. "1 \n 2" parses to ('1', '2')

> pair = spacedChar `above` spacedChar

> pairTest = doParse pair "1 \n 2"

Composes two vertical pairs of characters together horizontally
e.g. "1 3\n2 4" parses to ('1','2') and ('3','4')

> twoPair = do
>     p1 <- spacedChar `above` spacedChar
>     p2 <- spacedChar `above` spacedChar
>     return (p1, p2)

> twoPairTest = doParse twoPair "1 3\n2 4"

Composes two vertical pairs horizontally with " = char" on the second line
note use of nothing1 parser (expects 1 or more empty lines)
e.g. "1 3\n2 4 = x" parsers to (('1', '2'), ('3', '4'), 'x')

> pairsEqual = do
>     (p1, p2) <- twoPair
>     res <- nothing1 `above` do { char '='; spacedChar }
>     return (p1, p2, res)

> pairsEqualTest = doParse pairsEqual "1 3\n 2 4 = x"

Composes two vertical pairs horizontally with " = char" on first OR second line
note use of nothing parser (expects 0 or more empty lines)
E.g. accepts both "1 3 = x\n2 4" and "1 3\n2 4 = x"

> pairsEqual' = do
>     (p1, p2) <- twoPair
>     res <- nothing `above` do { char '='; spacedChar }
>     return (p1, p2, res)

> pairsEqualTest1 = doParse pairsEqual' "1 3 = x\n 2 4"
> pairsEqualTest2 = doParse pairsEqual' "1 3\n2 4 = x"

Compose three pattersn horizontally, a vertical pair, a dot on the 
second line then another vertical pair 
e.g. "x   u\ny . v" parses to (('x', 'y'),("", '.'),('u', 'v'))

> pairDotPair = do
>   p1 <- pair
>   dot <- (string "") `above` (char '.')
>   p2 <- pair
>   return (p1, dot, p2)

> pairDotPairTest = doParse pairDotPair "x   u\ny . v"

Compose a pair with hat on the second line
e.g. "1\n2 hat" parses to (('1', '2'), ((), "hat"))

> box1 = do
>              p1 <- pair
>              p2 <- nothing1 `above` string "hat"
>              return (p1, p2)

> box1Test = doParse box1 "1\n2 hat"

Composes the above vertically with "hello"
e.g. "1\n2 hat\nhello" parses to ((('1', '2'), ((), "hat")), ((), "hello"))

> box2 = box1 `above` (string "hello")

> box2Test = doParse box2 "1\n2 hat\nhello"

Composes box2 above another pair
e.g. "1\n2 hat\nhello\n3\n4" parses to
(((('1', '2'), ((), "hat")), ((), "hello")), ('3', '4'))

> box3 = box2 `above` pair

> box3Test = doParse box3 "1\n2 hat\nhello\n3\n4"

Used for testing parsers (note that Nothing is parsed in as state)

> doParse p input = 
>    case (runParser p Nothing "" input) of
>      Left err -> do { putStr "parse error at "; print err; return Nothing }
>      Right x -> return (Just x)
