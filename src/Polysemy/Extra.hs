{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators  #-}
module Polysemy.Extra where

import Data.Map as Map
import Polysemy
import Polysemy.KVStore
import Polysemy.Input
import Polysemy.Output

-- | Run a KVStore in terms of another KVStore by way of pure key and value
-- transformations.
runKVStoreAsKVStore :: forall k v k' v' r a.
                       (k  -> k')
                    -> (v  -> v')
                    -> (v' -> v )
                    -> Sem (KVStore k v ': r) a
                    -> Sem (KVStore k' v' ': r) a
runKVStoreAsKVStore f g h = reinterpret \case
  LookupKV k   -> fmap h <$> lookupKV @k' @v' (f k)
  UpdateKV k x -> updateKV @k' @v' (f k) (fmap g x)

-- | Run a KVStore in terms of another KVStore by way of transforming the
-- keys and values with Sem functions.
runKVStoreAsKVStoreSem :: forall k v k' v' r a.
                          Members '[KVStore k' v'] r
                       => (k  -> Sem r k')
                       -> (v  -> Sem r v')
                       -> (v' -> Sem r v )
                       -> Sem (KVStore k v ': r) a
                       -> Sem r a
runKVStoreAsKVStoreSem f g h = interpret \case
  LookupKV k   -> f k >>= lookupKV @k' @v' >>= mapM h
  UpdateKV k x -> do
    z  <- f k
    z' <- mapM g x
    updateKV @k' @v' z z'

-- | Run an `Output (Map k v)` as a `KVStore` by writing the values to
-- the keys.
runOutputMapAsKVStore :: Members '[ KVStore k v ] r
                      => Sem (Output (Map k v) ': r) a
                      -> Sem r a
runOutputMapAsKVStore = interpret \case
  Output xs -> mapM_ (uncurry writeKV) (Map.toList xs)

-- | Map an Output forwards 
mapOutput :: Members '[ Output o' ] r
          => (o -> o')
          -> Sem (Output o ': r) a
          -> Sem r a
mapOutput f = interpret \case
  Output o -> output (f o)

-- | Map an Output forwards through a monadic function.
mapOutputSem :: Members '[ Output o' ] r
             => (o -> Sem r o')
             -> Sem (Output o ': r) a
             -> Sem r a
mapOutputSem f = interpret \case
  Output o -> f o >>= output

-- | Map an `Input` contravariantly.
contramapInput :: forall i i' r a.
            Members '[ Input i' ] r
         => (i' -> i)
         -> Sem (Input i ': r) a
         -> Sem r a
contramapInput f = interpret \case
  Input -> f <$> input @i'

-- | Map an `Input` contravariantly through a monadic function.
contramapInputSem :: forall i i' r a.
                     Members '[ Input i' ] r
                  => (i' -> Sem r i)
                  -> Sem (Input i ': r) a
                  -> Sem r a
contramapInputSem f = interpret \case
  Input -> f =<< input @i'
