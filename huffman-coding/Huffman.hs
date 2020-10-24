-- Huffman Coding (from SICP p161)
module Huffman
  ( mkHuffmanTree
  , encode
  , decode
  ) where

import Data.List (sort, sortBy)
import qualified Data.Tree as T
import Data.Ord (comparing)


main :: IO ()
main =
  let msg = "BACADAEAFABBAAAGAH"
      ht = mkHuffmanTree msg
      encoded = encode ht msg
      encodedStr = concat $ map show encoded
      decoded = decode ht encoded
  in do
    putStrLn $ "Encoding message: " ++ show msg
    putStrLn $ "\tResult: " ++ encodedStr
    putStrLn $ "Decoding:"
    putStrLn $ "\tResult: " ++ show decoded


-- A Huffman Tree has nodes that contain left, right
-- and a value, which consists of the characters it
-- represents and their frequency.
data HTree
  = Node HTree HTree (String, Int)
  | Empty
  deriving (Show)


mkHuffmanTree :: String -> HTree
mkHuffmanTree text =
  let
    -- Calculate the frequencies
    fs = charFrequency text

    -- Map them to HTree notes
    nodes = map
      (\(c, f) -> Node Empty Empty ([c], f))
      fs

    -- Combine two nodes
    combine l@(Node _ _ (c1, f1)) r@(Node _ _ (c2, f2)) =
      Node l r (sort $ c1 ++ c2, f1 + f2)

    -- Sort the nodes by frequency
    sortNodes = sortBy (comparing (\(Node _ _ (_, f)) -> f))

    -- At each step, sort and then combine the two nodes with
    -- the lowest frequencies
    build []       = Empty
    build (x:[])   = x
    build (x:y:xs) = build (sortNodes (combine x y : xs))
  in
    build nodes


encode :: HTree -> String -> [Int]
encode ht text =
  let
    containedIn c (Node _ _ (n, _)) = c `elem` n
    containedIn c Empty             = False

    encode' (Node l r (n, f)) char =
      if [char] == n then
        []
      else if char `containedIn` l then
        0:(encode' l char)
      else if char `containedIn` r then
        1:(encode' r char)
      else
        error $ "Invalid tree does not contain char: " ++ show char
  in concat $ map (encode' ht) text


decode :: HTree -> [Int] -> String
decode ht bits =
  let
    decode' (Node Empty Empty (n, _)) bs = n ++ decode' ht bs
    decode' (Node l _ _) (0:bs) = decode' l bs
    decode' (Node _ r _) (1:bs) = decode' r bs
    decode' _ [] = ""
  in decode' ht bits


-- Emits a list of pairs (char, count) for a givenString
charFrequency :: String -> [(Char, Int)]
charFrequency =
  let
    charFrequency' [] = []

    charFrequency' (x:xs) =
      let same = takeWhile (== x) xs
          diff = dropWhile (== x) xs
      in (x, 1 + length same) : charFrequency' diff
  in charFrequency' . sort


-- printTree $ mkHuffmanTree "BACADAEAFABBAAAGAH"
-- Util to visualize trere
printTree :: HTree -> IO ()
printTree =
  let
    mkChildren Empty Empty = []
    mkChildren Empty r     = [mkTree r]
    mkChildren l     Empty = [mkTree l]
    mkChildren l     r     = [mkTree l, mkTree r]

    mkTree (Node l r (n, f)) =
      T.Node (n ++ " " ++ show f) (mkChildren l r)
  in
    putStr . T.drawTree . mkTree

