{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- Ugh this is disgusting but I'm tired.

{-

Write a function (dollars) that accepts a `String` and returns a `String`.
It will accept a numeric value as input, representing an amount of money, and convert to its transcribed English.

For example, the input "1.11" will result in a return value of "one dollar and eleven cents"

Invalid characters should be ignored, meaning that every input string has an output string.
The empty string produces "zero dollars and zero cents"

There is a `test` function below that lists more examples of input and output. There are also functions and
data structures that may assist you in deriving the result. It is not compulsory that they are used.

-}

module Course.Cheque where

import Course.Core
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad

-- $setup
-- >>> :set -XOverloadedStrings

-- The representation of the grouping of each exponent of one thousand. ["thousand", "million", ...]
illion ::
  List Chars
illion =
  let preillion ::
        List (Chars -> Chars)
      preillion =
        listh [
          const ""
        , const "un"
        , const "do"
        , const "tre"
        , const "quattuor"
        , const "quin"
        , const "sex"
        , const "septen"
        , const "octo"
        , \q -> if "n" `isPrefixOf` q then "novem" else "noven"
        ]
      postillion ::
        List Chars
      postillion =
        listh [
          "vigintillion"
        , "trigintillion"
        , "quadragintillion"
        , "quinquagintillion"
        , "sexagintillion"
        , "septuagintillion"
        , "octogintillion"
        , "nonagintillion"
        , "centillion"
        , "decicentillion"
        , "viginticentillion"
        , "trigintacentillion"
        , "quadragintacentillion"
        , "quinquagintacentillion"
        , "sexagintacentillion"
        , "septuagintacentillion"
        , "octogintacentillion"
        , "nonagintacentillion"
        , "ducentillion"
        , "deciducentillion"
        , "vigintiducentillion"
        , "trigintaducentillion"
        , "quadragintaducentillion"
        , "quinquagintaducentillion"
        , "sexagintaducentillion"
        , "septuagintaducentillion"
        , "octogintaducentillion"
        , "nonagintaducentillion"
        , "trecentillion"
        , "decitrecentillion"
        , "vigintitrecentillion"
        , "trigintatrecentillion"
        , "quadragintatrecentillion"
        , "quinquagintatrecentillion"
        , "sexagintatrecentillion"
        , "septuagintatrecentillion"
        , "octogintatrecentillion"
        , "nonagintatrecentillion"
        , "quadringentillion"
        , "deciquadringentillion"
        , "vigintiquadringentillion"
        , "trigintaquadringentillion"
        , "quadragintaquadringentillion"
        , "quinquagintaquadringentillion"
        , "sexagintaquadringentillion"
        , "septuagintaquadringentillion"
        , "octogintaquadringentillion"
        , "nonagintaquadringentillion"
        , "quingentillion"
        , "deciquingentillion"
        , "vigintiquingentillion"
        , "trigintaquingentillion"
        , "quadragintaquingentillion"
        , "quinquagintaquingentillion"
        , "sexagintaquingentillion"
        , "septuagintaquingentillion"
        , "octogintaquingentillion"
        , "nonagintaquingentillion"
        , "sescentillion"
        , "decisescentillion"
        , "vigintisescentillion"
        , "trigintasescentillion"
        , "quadragintasescentillion"
        , "quinquagintasescentillion"
        , "sexagintasescentillion"
        , "septuagintasescentillion"
        , "octogintasescentillion"
        , "nonagintasescentillion"
        , "septingentillion"
        , "deciseptingentillion"
        , "vigintiseptingentillion"
        , "trigintaseptingentillion"
        , "quadragintaseptingentillion"
        , "quinquagintaseptingentillion"
        , "sexagintaseptingentillion"
        , "septuagintaseptingentillion"
        , "octogintaseptingentillion"
        , "nonagintaseptingentillion"
        , "octingentillion"
        , "decioctingentillion"
        , "vigintioctingentillion"
        , "trigintaoctingentillion"
        , "quadragintaoctingentillion"
        , "quinquagintaoctingentillion"
        , "sexagintaoctingentillion"
        , "septuagintaoctingentillion"
        , "octogintaoctingentillion"
        , "nonagintaoctingentillion"
        , "nongentillion"
        , "decinongentillion"
        , "vigintinongentillion"
        , "trigintanongentillion"
        , "quadragintanongentillion"
        , "quinquagintanongentillion"
        , "sexagintanongentillion"
        , "septuagintanongentillion"
        , "octogintanongentillion"
        , "nonagintanongentillion"
        ]
  in listh [
       ""
     , "thousand"
     , "million"
     , "billion"
     , "trillion"
     , "quadrillion"
     , "quintillion"
     , "sextillion"
     , "septillion"
     , "octillion"
     , "nonillion"
     , "decillion"
     , "undecillion"
     , "duodecillion"
     , "tredecillion"
     , "quattuordecillion"
     , "quindecillion"
     , "sexdecillion"
     , "septendecillion"
     , "octodecillion"
     , "novemdecillion"
     ] ++ lift2 ((++) =<<) preillion postillion

-- A data type representing the digits zero to nine.
data Digit =
    Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  deriving (Eq, Ord)

showDigit :: Digit -> Chars
showDigit Zero = "zero"
showDigit One = "one"
showDigit Two = "two"
showDigit Three = "three"
showDigit Four = "four"
showDigit Five = "five"
showDigit Six = "six"
showDigit Seven = "seven"
showDigit Eight = "eight"
showDigit Nine = "nine"

-- A data type representing one, two or three digits, which may be useful for grouping.
data Digit3 =
    D1 Digit
  | D2 Digit Digit
  | D3 Digit Digit Digit
  deriving Eq

-- Possibly convert a character to a digit.
fromChar :: Char -> Optional Digit
fromChar '0' = Full Zero
fromChar '1' = Full One
fromChar '2' = Full Two
fromChar '3' = Full Three
fromChar '4' = Full Four
fromChar '5' = Full Five
fromChar '6' = Full Six
fromChar '7' = Full Seven
fromChar '8' = Full Eight
fromChar '9' = Full Nine
fromChar _   = Empty

data Money = Money (List Digit3) Digit3

-- | Take a numeric value and produce its English output.
--
-- >>> dollars "0"
-- "zero dollars and zero cents"
--
-- >>> dollars "1"
-- "one dollar and zero cents"
--
-- >>> dollars "0.1"
-- "zero dollars and ten cents"
--
-- >>> dollars "1."
-- "one dollar and zero cents"
--
-- >>> dollars "0."
-- "zero dollars and zero cents"
--
-- >>> dollars "0.0"
-- "zero dollars and zero cents"
--
-- >>> dollars ".34"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "0.3456789"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "1.0"
-- "one dollar and zero cents"
--
-- >>> dollars "1.01"
-- "one dollar and one cent"
--
-- >>> dollars "a1a"
-- "one dollar and zero cents"
--
-- >>> dollars "a1a.a0.7b"
-- "one dollar and seven cents"
--
-- >>> dollars "100"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.0"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00000"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "1000456.13"
-- "one million four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "1001456.13"
-- "one million one thousand four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "16000000456.13"
-- "sixteen billion four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "100.45"
-- "one hundred dollars and forty-five cents"
--
-- >>> dollars "100.07"
-- "one hundred dollars and seven cents"
--
-- >>> dollars "9abc9def9ghi.jkl9mno"
-- "nine hundred and ninety-nine dollars and ninety cents"
--
-- >>> dollars "12345.67"
-- "twelve thousand three hundred and forty-five dollars and sixty-seven cents"
--
-- >>> dollars "456789123456789012345678901234567890123456789012345678901234567890.12"
-- "four hundred and fifty-six vigintillion seven hundred and eighty-nine novemdecillion one hundred and twenty-three octodecillion four hundred and fifty-six septendecillion seven hundred and eighty-nine sexdecillion twelve quindecillion three hundred and forty-five quattuordecillion six hundred and seventy-eight tredecillion nine hundred and one duodecillion two hundred and thirty-four undecillion five hundred and sixty-seven decillion eight hundred and ninety nonillion one hundred and twenty-three octillion four hundred and fifty-six septillion seven hundred and eighty-nine sextillion twelve quintillion three hundred and forty-five quadrillion six hundred and seventy-eight trillion nine hundred and one billion two hundred and thirty-four million five hundred and sixty-seven thousand eight hundred and ninety dollars and twelve cents"
dollars :: Chars -> Chars
dollars s = formatDollars d ++ " and " ++ formatCents c
  where
    Money d c = parseMoney s

-- | Parse the stream of digits into cash money. Do we not have parser
-- module for this??
parseMoney :: Chars -> Money
parseMoney s = Money (groupDollars dollarsDigits) cents
  where 
    (dollarsDigits, s') = parseDollars s
    cents               = parseCents s'

-- | Parse dollars from lowest weight to highest. Return the remaining characters.
parseDollars :: Chars -> (List Digit, Chars)
parseDollars = go Nil
  where
    go digits Nil       = (digits, Nil)
    go digits ('.':.xs) = (digits, xs)
    go digits (x  :.xs) = case fromChar x of
                            Full d -> go (d:.digits) xs
                            Empty  -> go digits      xs

-- | Parse cents, will always return a `D2`.
parseCents :: Chars -> Digit3
parseCents s =
  case fromChar <$> filter isDigit s of
    Full x :. Nil         -> D2 x Zero
    Full x :. Full y :. _ -> D2 x y
    _                     -> D2 Zero Zero

-- | Group a list of dollars (min-weight to max weight) into relevant
-- Digit3 groupings, still from min-weight to max-weight.
groupDollars :: List Digit -> List Digit3
groupDollars = go
  where
    go (x:.y:.z:.xs ) = D3 z y x :. go xs
    go (   y:.z:.Nil) = D2 z y   :. Nil
    go (      z:.Nil) = D1 z     :. Nil
    go           Nil  = Nil

formatDollars :: List Digit3 -> Chars
formatDollars ds = formattedOutput ++ "dollar" ++ trailingS
  where
    formattedOutput          =
      let o = join . reverse $ formatDollar <$> zip ds illion
      in if isEmpty o then "zero " else o
    formatDollar (d, suffix) = let o = formatD d
                                in if o == "zero" && not (isEmpty suffix)
                                   then ""
                                   else if isEmpty suffix
                                        then o ++ " "
                                        else o ++ " " ++ suffix ++ " "
    trailingS                = if formattedOutput == "one " then "" else "s"

-- | Format cents.
formatCents :: Digit3 -> Chars
formatCents d = formattedOutput ++ " cent" ++ trailingS
  where
    formattedOutput = formatD d
    trailingS       = if formattedOutput == "one" then "" else "s"

-- | Format Digit3.
formatD :: Digit3 -> Chars
formatD (D1 x        ) = showDigit x
formatD (D2 Zero y   ) = showDigit y
formatD (D2 One  y   ) = showTeens y
formatD (D2 x    Zero) = showTens x
formatD (D2 x    y   ) = showTens x ++ "-" ++ showDigit y
formatD (D3 Zero y z ) = formatD (D2 y z)
formatD (D3 x    y z ) = let tens = if y /= Zero || z /= Zero
                                   then " and " ++ formatD (D2 y z)
                                   else ""
                        in showDigit x ++ " hundred" ++ tens

showTeens :: Digit -> Chars
showTeens Zero = "ten"
showTeens One = "eleven"
showTeens Two = "twelve"
showTeens Three = "thirteen"
showTeens Four = "fourteen"
showTeens Five = "fifteen"
showTeens Six = "sixteen"
showTeens Seven = "seventeen"
showTeens Eight = "eighteen"
showTeens Nine = "nineteen"

showTens :: Digit -> Chars
showTens Zero = error "GO away there is no zeroth tens!"
showTens One = error "GO away are you sure you didnt means showTeens!"
showTens Two = "twenty"
showTens Three = "thirty"
showTens Four = "forty"
showTens Five = "fifty"
showTens Six = "sixty"
showTens Seven = "seventy"
showTens Eight = "eighty"
showTens Nine = "ninety"
