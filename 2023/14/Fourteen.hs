{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

import qualified System.Environment as Env
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.HashSet as H
import qualified Data.List as L
import Data.Hashable (Hashable)

pattern x :> xs <- (Text.uncons -> Just (x, xs))
pattern Empty <- (Text.uncons -> Nothing)

rot90 :: [Text.Text] -> [Text.Text]
rot90 = reverse . Text.transpose

rot270 :: [Text.Text] -> [Text.Text]
rot270 = Text.transpose . reverse

west :: [Text.Text] -> [Text.Text]
west = map $ west' 0
    where
        west' n Empty = Text.replicate n (Text.pack ".")
        west' n ('.':>rs) = west' (n + 1) rs
        west' n ('#':>rs) = Text.append (Text.snoc (Text.replicate n (Text.pack ".")) '#') (west' 0 rs)
        west' n ('O':>rs) = Text.cons 'O' (west' n rs)

weight :: [Text.Text] -> Int
weight =  sum . map (weight' 0 0)
    where
        weight' n t Empty = t
        weight' n t ('.':>rs) = weight' n (t + n) rs
        weight' n t ('#':>rs) = weight' n (t + n) rs
        weight' n t ('O':>rs) = weight' (n + 1) (t + n + 1) rs

readLines :: String -> IO [Text.Text]
readLines file = Text.lines <$> Text.readFile file

input :: IO [Text.Text]
input = readLines . head =<< Env.getArgs

cycle' :: [Text.Text] -> [Text.Text]
cycle' = fpow 4 (rot270 . west)

firstDup :: (Hashable a, Eq a) => [a] -> (a, Int, Int)
firstDup gs = (dup, first, second)
    where
        [first, second] = L.elemIndices dup (take pos gs)
        (dup, pos) = nfirst gs H.empty 0
        nfirst (x:xs) hs 0 = nfirst xs (H.singleton x) 1
        nfirst (x:xs) hs n = let nhs = H.insert x hs in if H.size nhs < (n + 1) then (x, n + 1) else nfirst xs nhs (n + 1) 

fpow :: Int -> (a -> a) -> a -> a
fpow 0 f x = x
fpow n f x = fpow (n - 1) f (f x)

main :: IO ()
main = do
    sample <- input
    let
        start = west . rot90 $ sample
        -- Identify a cycle in the list of generated configurations
        (dup, first, second) = firstDup (iterate cycle' start)
        rem = (1000000000 - first) `mod` (second - first)
    print (weight start)
    print (weight $ fpow rem cycle' dup)