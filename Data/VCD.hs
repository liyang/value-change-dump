{-# LANGUAGE ScopedTypeVariables #-}

module Data.VCD
    ( module Data.VCD.Types
    , module Data.VCD
    ) where

import Prelude

import Control.Applicative
import Control.Monad.Trans.State.Strict
import qualified Data.Attoparsec.ByteString.Char8 as AP
import Data.ByteString.Char8 (ByteString)

import Pipes
import Pipes.Lift
import qualified Pipes.Attoparsec as PAP

import qualified Data.VCD.Parser as AP
import Data.VCD.Types

-- | 'Value' eliminator.
value :: (Four -> a) -> ([Four] -> a) -> (Double -> a) -> Value -> a
value scalar vector real v = case v of
    VScalar x -> scalar x
    VVector xs -> vector xs
    VReal n -> real n

parse :: forall m. (Monad m) => Producer ByteString m () ->
    Producer Declaration m (Producer Simulation m (Maybe PAP.ParsingError))
parse = flip evalStateP decs where
    -- NOTE: 'Pipes.Parse.Parser' has to be saturated
    -- type Parser a m = StateT (Producer a m ()) m

    decs :: Producer Declaration (StateT (Producer ByteString m ()) m)
        (Producer Simulation m (Maybe PAP.ParsingError))
    decs = do
        mee'dec <- lift . PAP.parse $ AP.skipSpace
            *> AP.eitherP AP.declaration AP.endDefinitions
        either' (seqL mee'dec) (return . return) $ \ e'dec -> case e'dec of
            Left dec -> do yield dec; decs
            Right () -> return . flip evalStateP sims =<< lift get

    -- FIXME: distinguish between EOF and parse error?
    sims :: Producer Simulation (StateT (Producer ByteString m ()) m)
        (Maybe PAP.ParsingError)
    sims = do
        me'sim <- lift . PAP.parse $ AP.skipSpace *> AP.simulation
        either' (seqL me'sim) return $ \ sim -> do yield sim; sims

    -- | Like 'sequenceA', but on 'Left's of 'Either'.
    seqL :: Maybe (Either l r) -> Either (Maybe l) r
    seqL = maybe (Left Nothing) (either (Left . Just) Right)

    -- | Flipped 'either'.
    either' :: Either l r -> (l -> a) -> (r -> a) -> a
    either' e l r = either l r e

changes :: (Monad m) =>
    (IdCode -> Value -> Maybe a) ->
    Pipe Simulation (Time, a) m r
changes f = evalStateP 0 . for cat $ \ s -> case s of
    SComment _ -> return ()
    SDump _ cs -> mapM_ g cs
    STime t -> lift $ put t
    SChange c -> g c
  where
    g (Change n v) = maybe (return ())
        (\ a -> do t <- lift get; yield (t, a)) (f n v)

states :: (Monad m) => Producer (Time, a) m r -> Producer (Duration, a) m r
states = (=<<) (either return go) . lift . next where
    go ((t0, a0), cs) = do
        ff <- lift (next cs)
        case ff of
            Left r -> r <$ yield (maxBound, a0) -- last state forever
            Right tap1@((t1, _), _) -> do
                yield (t1 - t0, a0)
                go tap1

