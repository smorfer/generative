{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.Word
import qualified Data.Array.Repa               as Repa
import qualified Data.Array.Repa.Index         as RIndex
import qualified Data.Array.Repa.IO.BMP        as RBMP

main :: IO ()
main = do
  putStrLn "Generative library R&D"
  let size          = 512
      list :: [Int] = [0 .. (size ^ 2 - 1)]
      repaArr :: Repa.Array Repa.U RIndex.DIM2 Int =
        Repa.fromListUnboxed (RIndex.ix2 size size) list
  computed <- Repa.computeUnboxedP $ Repa.map toPixel repaArr

  RBMP.writeImageToBMP
    ("computed/gradient" ++ show size ++ "*" ++ show size ++ ".bmp")
    computed
  return ()
 where
  toPixel :: Int -> (Word8, Word8, Word8)
  toPixel n = (fromIntegral n, fromIntegral (n + 128), fromIntegral (n + 64))
