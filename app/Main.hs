{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.Time.Clock
import           Debug.Trace
import           Data.Word
import           Data.List.Split
import qualified Data.Array.Repa               as Repa
import qualified Data.Array.Repa.Index         as RIndex
import qualified Data.Array.Repa.IO.BMP        as RBMP

main :: IO ()
main = do
  putStrLn "Generative library R&D"
  let sizex                              = 256
      sizey                              = 256
      lists :: [[(Word8, Word8, Word8)]] = chunksOf
        65536
        [ (x, y, z)
        | x <- [0 .. 255] :: [Word8]
        , y <- [0 .. 255] :: [Word8]
        , z <- [0 .. 255] :: [Word8]
        ]
  print (length $ head lists)
  let repaArr :: [Repa.Array Repa.U RIndex.DIM2 (Word8, Word8, Word8)] =
        Repa.fromListUnboxed (RIndex.ix2 sizex sizey) <$> lists
  -- computed <- Repa.computeUnboxedP $ Repa.map toPixel repaArr
  putStrLn "Writing image"
  sequence_
    $ let f x = do
            time <- getCurrentTime
            RBMP.writeImageToBMP
              (  "computed/collection_testing/"
              ++ show time
              ++ show sizex
              ++ "*"
              ++ show sizey
              ++ ".bmp"
              )
              x
      in  f <$> repaArr

 where
  toPixel :: Int -> (Word8, Word8, Word8)
  toPixel n =
    trace
        ("Step: " ++ show n)
        [ (x, y, z)
        | x <- [0 .. 255] :: [Word8]
        , y <- [0 .. 255] :: [Word8]
        , z <- [0 .. 255] :: [Word8]
        ]
      !! n
