Provides a 2D parser combinator to ParSec 
Dominic Orchard
http://github.com/dorchard/parsec2D/

> module Text.ParserCombinators.Parsec.TwoD (nothing, nothing1, nothingWeak, above) where
> import Text.ParserCombinators.Parsec
> import Data.List

above combinators must be used with runParser given an initial Maybe state
aboveStrict assumes that newlines are never parsed explicitly but the
above combinator is always used instead

> above = aboveStrict

> aboveStrict :: GenParser Char (Maybe Int) a -> GenParser Char (Maybe Int) b -> GenParser Char (Maybe Int) (a, b)
> aboveStrict p1 p2 =
>     do

        Parse the first pattern

>       setState Nothing
>       pos1 <- getPosition
>       input1 <- getInput
>       t1 <- p1 <?> "Failed on upper part of `above` on input "++(show input1)
>       pos2 <- getPosition
>       input2 <- getInput
>       state2 <- getState

        Set remaining input to the lines below those consumed by p1
        Keep track of partial lines left to the right of p1 in remainder1

>       let n = case state2 of 
>                 Nothing -> 1
>                 Just x -> x

>       let ys = lines input2
>       let remainder1 = (take n ys)
>       let input2' = intercalate "\n" (drop n ys)

>       setInput input2'
>       setPosition (setSourceLine pos2 ((sourceLine pos2)+1))

        Parse the second pattern

>       setState Nothing
>       t2 <- p2 <?> "Failed on bottom part of `above` on input "++(show input2')
>       pos3 <- getPosition
>       input3 <- getInput
>       state3 <- getState

        Compute the remaining input afer p1 above p2 has been parsed
        This may include some partial lines to the right of p1 and p2        

>       let m = case state3 of
>                 Nothing -> 1
>                 Just x -> x
>       let input4 = (intercalate "\n" remainder1)++"\n"++input3

>       setInput input4
>       setPosition pos2

        Finally update the state with numer of lines parsed by p1 and p2

>       setState (Just (n+m))
>       return (t1, t2)


Parse zero or more blank lines from the current position upwards

> nothing = do
>   input <- getInput
>   let ys = lines input
>   pos <- getPosition
>   let linePos = sourceLine pos
>   d <- return $ countPrefixNewlines (reverse (take linePos ys))
>   setState (Just d)
>   

Parses one or more blank lines from the current position upwards

> nothing1 = do
>   input <- getInput
>   let ys = lines input
>   pos <- getPosition
>   let linePos = sourceLine pos
>   d <- return $ countPrefixNewlines (reverse (take linePos ys))
>   if (d==0) then fail "nothing1 expected to see a line terminated with a newline"
>      else setState (Just d)

> nothingWeak = string ""

Used by nothing to determine the length of a blank line prefix on the block of lines

> countPrefixNewlines :: [String] -> Int
> countPrefixNewlines [] = 0
> countPrefixNewlines (x:xs) | x==""   = 1+(countPrefixNewlines xs)
>                            | otherwise = 0