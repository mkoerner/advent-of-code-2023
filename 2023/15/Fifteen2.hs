{-# LANGUAGE ViewPatterns #-}

import qualified System.Environment as Env
import Data.List (groupBy, sortBy)
import Data.List.Split (splitOn)
import Data.Foldable (foldl')
import Data.Char (ord)

input :: IO [String]
input = lines <$> (readFile . head =<< Env.getArgs)

data Op = 
    Add Int String Int
  | Del Int String
  deriving (Show)

box :: Op -> Int
box (Add b _ _) = b
box   (Del b _) = b

sameBox :: Op -> Op -> Bool
sameBox l r = box l == box r

label :: Op -> String
label (Add _ l _) = l
label   (Del _ l) = l

hash :: String -> Int
hash = foldl' (\h c -> (17 * (h + ord c)) `mod` 256) 0

decode :: String -> Op
decode   (reverse -> '-':xs) = Del (hash $ reverse xs) (reverse xs)
decode (reverse -> c:'=':xs) = Add (hash $ reverse xs) (reverse xs) (read [c])

run' :: [Op] -> [Op] -> [Op]
run' curr (a@(Add _ l _):xs) = run' (if any (\x -> label x == l) curr then map (\x -> if label x == l then a else x) curr else a:curr) xs
run' curr       (Del _ l:xs) = run' (filter (\x -> label x /= l) curr) xs
run' curr                 [] = curr

focus :: Int -> Op -> Int
focus slot (Add b _ f) = slot * (b + 1) * f

main :: IO ()
main = print . sum . map (sum . zipWith focus [1..] . reverse . run' []) . groupBy sameBox . sortBy (\l r -> compare (box l) (box r)). map decode . splitOn "," . head =<< input
