{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Daikin Aircon Remote Protocol Decoder
module Main where

import Prelude

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString as BS
import Data.Foldable
import Data.List
import Data.Word
import Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as PBS
import System.Environment
import System.IO
import Text.Printf

import Data.VCD

type DF = (Duration, Four)

df :: Duration -> Four -> DF -> Bool
df d f (d', f') = f == f' && (d - d' < w || d' - d < w) where w = shiftR d 4

-- FIXME: error or stop bit?
bool :: (Monad m) => Consumer' DF m (Either DF Bool)
bool = do
    peak <- await
    if not $ df 433 I peak then return (Left peak) else do
    trough <- await
    if df 433 O trough then return (Right False) else do
    if df 1300 O trough then return (Right True) else do
    return (Left trough)

-- FIXME: error or stop bit?
byte :: (Monad m) => Consumer' DF m (Either DF Word8)
byte = do
    bat $ \ b0 -> bat $ \ b1 -> bat $ \ b2 -> bat $ \ b3 -> do
    bat $ \ b4 -> bat $ \ b5 -> bat $ \ b6 -> bat $ \ b7 -> do
    return $ Right $ set 7 b7 .|. set 6 b6 .|. set 5 b5 .|. set 4 b4
        .|. set 3 b3 .|. set 2 b2 .|. set 1 b1 .|. set 0 b0
  where
    set i b = if b then bit i else zeroBits
    bat f = either (return . Left) f =<< bool

type Packet = ByteString

-- FIXME: error or stop bit?
packet :: (Monad m) => Pipe DF (Either DF Packet) m ()
packet = do
    peak <- await
    if not $ df 3467 I peak then packet else do
    trough <- await
    if not $ df 1733 O trough then return () else do
    gobble []
  where
    gobble bs@ ~(BS.pack . reverse -> pkt) = do
        e'b <- byte
        case e'b of
            Left e@(d, f) -> if d >= 3900 && f == O
                then yield (Right pkt) else yield (Left e)
            Right b -> gobble (b : bs)

{- decode :: Pipe ByteString THING m () -}

getInput :: IO (Producer ByteString IO ())
getInput = getArgs >>= \ args -> case args of
    [] -> return PBS.stdin
    [file] -> PBS.fromHandle <$> openBinaryFile file ReadMode
    _ -> undefined

main :: IO ()
main = do
    input <- getInput
    (decs, sims) <- P.fold' (flip (:)) [] id (parse input)
    for_ decs print

    m'e <- runEffect $ states (sims >-> changes sel)
        >-> forever packet
        >-> P.map (either show hex) >-> (Nothing <$ P.stdoutLn)
    for_ m'e print

  where
    sel i = if i /= "!" then const Nothing
        else value Just (const Nothing) (const Nothing)
    hex = intercalate " " . map (printf "%02x") . BS.unpack

