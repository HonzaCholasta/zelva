-- Copyright (c) 2018 Jan Cholasta <jan@cholasta.net>
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

module Turtle where

data TurtleCommand = TurtleCommand { name :: Char, value :: Maybe Float } deriving (Eq, Show)

data TurtleRule = TurtleRule { ruleFrom :: Char, ruleTo :: [TurtleCommand] } deriving (Eq, Show)

data Turtle = Turtle { recursion :: Int, angle :: Float, thickness :: Float, axiom :: [TurtleCommand], rules :: [TurtleRule] } deriving (Eq, Show)

data TurtleState = TurtleState { curAngle :: Float, curThickness :: Float, colorIndex :: Int } deriving (Eq, Show)

parseLSystem :: String -> Maybe Turtle
parseLSystem source =
    if (((fst parseRecursion) /= Nothing) &&
        ((fst parseAngle) /= Nothing) &&
        ((fst parseThickness) /= Nothing) &&
        ((fst parseAxiom) /= Nothing) &&
        ((fst parseRules) /= Nothing) &&
        ((last srcLines) == "@"))
        then Just Turtle {
            recursion = valueOf parseRecursion,
            angle = fromIntegral $ valueOf parseAngle,
            thickness = (fromIntegral $ valueOf parseThickness) / 100.0,
            axiom = valueOf parseAxiom,
            rules = valueOf parseRules}
        else Nothing
    where
        srcLines :: [String]
        srcLines = map concat $ filter (/= []) $ map (\x -> words $ takeWhile (/= '#') x) (lines source)

        valueOf :: (Maybe a, String) -> a
        valueOf (Just x, _) = x

        parseRecursion :: (Maybe Int, String)
        parseRecursion = parseInt $ srcLines !! 0

        parseAngle :: (Maybe Int, String)
        parseAngle     = parseInt $ srcLines !! 1

        parseThickness :: (Maybe Int, String)
        parseThickness = parseInt $ srcLines !! 2

        parseAxiom :: (Maybe [TurtleCommand], String)
        parseAxiom = parseCommands $ srcLines !! 3

        parseRules :: (Maybe [TurtleRule], String)
        parseRules = if (length $ filter (\x -> (fst x) == Nothing) list) == 0
            then (Just $ map valueOf list, "")
            else (Nothing, "")
            where
                list = map (\i -> parseRule $ srcLines !! i) [4..((length srcLines) - 2)]

        parseInt :: String -> (Maybe Int, String)
        parseInt "" = (Nothing, "")
        parseInt s  = tr $ (reads :: ReadS Int) s
            where
                tr :: [(Int, String)] -> (Maybe Int, String)
                tr [(i, "")] = (Just i, "")
                tr _         = (Nothing, s)

        parseCommand :: String -> (Maybe TurtleCommand, String)
        parseCommand s@(_:'(':xs) = tr1 s (span (/= ')') xs)
            where
                tr1 :: String -> (String, String) -> (Maybe TurtleCommand, String)
                tr1 s (_, "")         = (Nothing, s)
                tr1 s ("", _)         = (Nothing, s)
                tr1 s (s1, ')':xs)    = tr2 s ((reads :: ReadS Float) ('0':s1)) xs
                tr2 :: String -> [(Float, String)] -> String -> (Maybe TurtleCommand, String)
                tr2 s [] _            = (Nothing, s)
                tr2 (x:_) [(v, "")] r = (Just $ TurtleCommand x (Just v), r)
                tr2 s _ _             = (Nothing, s)
        parseCommand (x:xs)       = (Just $ TurtleCommand x Nothing, xs)
        parseCommand s            = (Nothing, s)


        parseCommands :: String -> (Maybe [TurtleCommand], String)
        parseCommands s = rec1 s (Just [])
            where
                rec1 :: String -> Maybe [TurtleCommand] -> (Maybe [TurtleCommand], String)
                rec1 "" l                 = (l, "")
                rec1 s l                  = rec2 (parseCommand s) l
                rec2 :: (Maybe TurtleCommand, String) -> Maybe [TurtleCommand] -> (Maybe [TurtleCommand], String)
                rec2 (Just c, s) (Just l) = rec1 s (Just (l ++ [c]))
                rec2 (Nothing, "") l      = (l, s)
                rec2 (Nothing, s) l       = (Nothing, s)

        parseRule :: String -> (Maybe TurtleRule, String)
        parseRule (x:'=':xs) = (maybe Nothing result (fst cl), (snd cl))
            where
                cl = parseCommands xs
                result y = Just $ TurtleRule x y
        parseRule s          = (Nothing, s)

ruleLookup :: Turtle -> Char -> Maybe [TurtleCommand]
ruleLookup turtle cmd = rl cmd (rules turtle)
    where
        rl :: Char -> [TurtleRule] -> Maybe [TurtleCommand]
        rl _ [] = Nothing
        rl x ((TurtleRule c l):ys)
            | x == c    = Just l
            | otherwise = rl x ys
