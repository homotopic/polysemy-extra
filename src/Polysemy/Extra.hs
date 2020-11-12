{-|
Module      : Polysemy.Extra
License     : MIT
Maintainer  : dan.firth@homotopic.tech
Stability   : experimental

Extra convenience functions for polysemy.
-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
module Polysemy.Extra (
-- * Input
  contramapInput
, contramapInputSem

-- * Output
, mapOutput
, mapOutputSem
, runOutputMapAsKVStore

-- * KVStore
, runKVStoreAsKVStore
, runKVStoreAsKVStoreSem

-- * Reinterpreters
, reinterpretUnder
, reinterpretUnder2
, reinterpret2Under

-- * Rotation
, rotateEffects2
, rotateEffects3L
, rotateEffects3R
) where

import Control.Arrow
import Data.Map as Map
import Polysemy
import Polysemy.KVStore
import Polysemy.Input
import Polysemy.Output
import Polysemy.Membership

-- | Run a `KVStore` in terms of another `KVStore` by way of pure key and value
-- transformations.
--
-- @since 0.1.0.0
runKVStoreAsKVStore :: forall k v k' v' r a.
                       (k  -> k')
                       -- ^ A function to transform the key into the interpreted key.
                    -> (v  -> v')
                       -- ^ A function to transform the value into the interpreted value.
                    -> (v' -> v )
                       -- ^ A function to transform the interpreted key back into the current value.
                    -> Sem (KVStore k v ': r) a
                    -> Sem (KVStore k' v' ': r) a
runKVStoreAsKVStore f g h = reinterpret \case
  LookupKV k   -> fmap h <$> lookupKV @k' @v' (f k)
  UpdateKV k x -> updateKV @k' @v' (f k) (fmap g x)

-- | Run a `KVStore` in terms of another `KVStore` by way of transforming the
-- keys and values with Sem functions.
--
-- @since 0.1.0.0
runKVStoreAsKVStoreSem :: forall k v k' v' r a.
                          Members '[KVStore k' v'] r
                       => (k  -> Sem r k')
                          -- ^ A function to transform the key into the interpreted key.
                       -> (v  -> Sem r v')
                          -- ^ A function to transform the value into the interpreted value.
                       -> (v' -> Sem r v )
                          -- ^ A function to transform the interpreted value back into the current value.
                       -> Sem (KVStore k v ': r) a
                       -> Sem r a
runKVStoreAsKVStoreSem f g h = interpret \case
  LookupKV k   -> f k >>= lookupKV @k' @v' >>= mapM h
  UpdateKV k x -> do
    z  <- f k
    z' <- mapM g x
    updateKV @k' @v' z z'

-- | Run an `Output` (`Map` k v) as a `KVStore` by writing the values to
-- the keys.
--
-- @since 0.1.0.0
runOutputMapAsKVStore :: Members '[ KVStore k v ] r
                      => Sem (Output (Map k v) ': r) a
                      -> Sem r a
runOutputMapAsKVStore = interpret \case
  Output xs -> mapM_ (uncurry writeKV) (Map.toList xs)

-- | Map an `Output` covariantly.
--
-- @since 0.1.0.0
mapOutput :: Members '[ Output o' ] r
          => (o -> o')
             -- ^ A function to map the old output to the new output.
          -> Sem (Output o ': r) a
          -> Sem r a
mapOutput f = interpret \case
  Output o -> output (f o)

-- | Map an `Output` covariantly through a monadic function.
--
-- @since 0.1.0.0
mapOutputSem :: Members '[ Output o' ] r
             => (o -> Sem r o')
                -- ^ A function to map the old output to the new output.
             -> Sem (Output o ': r) a
             -> Sem r a
mapOutputSem f = interpret \case
  Output o -> f o >>= output

-- | Map an `Input` contravariantly.
-- @since 0.1.0.0
contramapInput :: forall i i' r a.
            Members '[ Input i' ] r
         => (i' -> i)
            -- ^ A function to map the new input to the old input.
         -> Sem (Input i ': r) a
         -> Sem r a
contramapInput f = interpret \case
  Input -> f <$> input @i'

-- | Map an `Input` contravariantly through a monadic function.
--
-- @since 0.1.0.0
contramapInputSem :: forall i i' r a.
                     Members '[ Input i' ] r
                  => (i' -> Sem r i)
                     -- ^ A function to map the new input to the old input.
                  -> Sem (Input i ': r) a
                  -> Sem r a
contramapInputSem f = interpret \case
  Input -> f =<< input @i'

-- | Reinterpret the second effect in the stack into a single effect.
--
-- @since 0.1.1.0
reinterpretUnder :: forall e1 e2 e3 r a.
                    (forall m x. Sem (e2 ': m) x -> Sem (e3 ': m) x)
                    -- ^ A natural transformation from the handled effect to the new effects.
                 -> Sem (e1 ': e2 ': r) a
                 -> Sem (e1 ': e3 ': r) a
reinterpretUnder f = raise2Under @e1 @e1 @e2
                 >>> subsumeUsing @e1 (There Here)
                 >>> f
                 >>> raise2Under @e3 @e3 @e1
                 >>> subsumeUsing @e3 (There Here)

-- | Reinterpret the third effect in the stack into a single effect.
--
-- @since 0.1.1.0
reinterpretUnder2 :: forall e1 e2 e3 e4 r a.
                     (forall m x. Sem (e3 ': m) x -> Sem (e4 ': m) x)
                     -- ^ A natural transformation from the handled effect to the new effects.
                  -> Sem (e1 ': e2 ': e3 ': r) a
                  -> Sem (e1 ': e2 ': e4 ': r) a
reinterpretUnder2 f = raise3Under @e1 @e1 @e2 @e3
                  >>> subsumeUsing @e1 (There $ There Here)
                  >>> raise3Under @e2 @e2 @e3 @e1
                  >>> subsumeUsing @e2 (There $ There Here)
                  >>> f
                  >>> raise3Under @e4 @e4 @e1 @e2
                  >>> subsumeUsing @e4 (There $ There Here)

-- | Reinterpret the second effect in the stack in terms of two effects.
--
-- @since 0.1.1.0
reinterpret2Under :: forall e1 e2 e3 e4 r a.
                     (forall m x. Sem (e2 ': m) x -> Sem (e3 ': e4 ': m) x)
                     -- ^ A natural transformation from the handled effect to the new effects.
                  -> Sem (e1 ': e2 ': r) a
                  -> Sem (e1 ': e3 ': e4 ': r) a
reinterpret2Under f = raise2Under @e1 @e1 @e2
                  >>> subsumeUsing @e1 (There Here)
                  >>> f
                  >>> raise3Under @e3 @e3 @e4 @e1
                  >>> subsumeUsing @e3 (There $ There Here)
                  >>> raise3Under @e4 @e4 @e1 @e3
                  >>> subsumeUsing @e4 (There $ There Here)

-- | Swap the positions of the first two effects in the stack.
--
-- @since 0.1.2.0
rotateEffects2 :: forall e1 e2 r a. Sem (e1 ': e2 ': r) a -> Sem (e2 ': e1 ': r) a
rotateEffects2 = raise2Under >>> subsumeUsing (There Here)

-- | Rotate the first three effects in the stack to the left.
--
-- @since 0.1.2.0
rotateEffects3L :: forall e1 e2 e3 r a. Sem (e1 ': e2 ': e3 ': r) a -> Sem (e2 ': e3 ': e1 ': r) a
rotateEffects3L = raise3Under >>> subsumeUsing (There $ There Here)

-- | Rotate the first three effects in the stack to the right.
--
-- @since 0.1.2.0
rotateEffects3R :: forall e1 e2 e3 r a. Sem (e1 ': e2 ': e3 ': r) a -> Sem (e3 ': e1 ': e2 ': r) a
rotateEffects3R = rotateEffects3L >>> rotateEffects3L
