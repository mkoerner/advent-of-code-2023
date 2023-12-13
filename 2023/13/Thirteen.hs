import qualified System.Environment as Env
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.List as L
import qualified Data.List.Split as S
import Data.Bool (Bool)

canMirrorAt'' :: Text.Text -> Int -> Bool
canMirrorAt'' text i = all (uncurry (==)) $ Text.zip (Text.reverse hd) tl
    where 
        (hd, tl) = Text.splitAt i text

canMirrorAt' :: Text.Text -> [Int] -> [Int] 
canMirrorAt' text = filter (canMirrorAt'' text)

canMirrorAt :: [Text.Text] -> [Int] -> [Int]
canMirrorAt texts xs = L.foldl' (flip canMirrorAt') xs texts

mirror :: [Text.Text] -> Int
mirror text = sum (canMirrorAt text [1 .. Text.length (head text) - 1]) + 100 * sum (canMirrorAt trsp [1 .. Text.length (head trsp) - 1])
    where
        trsp = Text.transpose text

readLines :: String -> IO [Text.Text]
readLines file = Text.lines <$> Text.readFile file

input :: IO [[Text.Text]]
input = S.splitOn [Text.pack ""] <$> (readLines . head =<< Env.getArgs)

main :: IO ()
main = print . sum . map mirror =<< input
