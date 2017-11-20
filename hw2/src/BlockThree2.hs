{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}

module BlockThree1 where

import           BlockThree (Functor (..), Monad (..), MonadFish (..),
                             MonadJoin (..))
import           Prelude    (id, ($))

instance MonadFish m => MonadJoin m where
  returnJoin = returnFish
  join x = (id >=> id) x

    {- PROOF: join . returnJoin === id
       join (returnJoin x) === id x
       join (returnFish x) === id x
       (id >=> id) (returnFish x) === id x

    -}

instance (Functor m, MonadJoin m) => Monad m where
  return = returnJoin
  (>>=) x f = join $ fmap f x
    {- PROOF: m >>= return === m
       join $ fmap return m === m       (>>= definition)
       join $ fmap returnJoin m === m   (return definition)
       join $ fmap returnJoin m === id m
       join . fmap returnJoin === id    (eta reduction, MonadJoin law 3)
    -}

instance (Functor m, MonadJoin m) => MonadFish m where
  returnFish = returnJoin
  (>=>) f g = \x -> join (fmap g (f x))
    {- PROOF: f >=> returnFish === f
       \x -> join (fmap returnFish (f x)) === f
       join (fmap returnFish (f x)) === f x    (apply lambda)
       join (fmap returnJoin (f x)) === f x    (returnFish definition)
       (join . fmap returnJoin) (f x) === f x
       id (f x) === f x                        (MonadJoin law 2)
       f x === f x
    -}
