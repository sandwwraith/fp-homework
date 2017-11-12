{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}

module BlockThree1 where

import           BlockThree (Monad (..), MonadFish (..), MonadJoin (..))
import           Prelude    (id, (.))

instance Monad m => MonadFish m where
  returnFish = return
  (>=>) f g = \x -> f x >>= g
    {- PROOF: f >=> returnFish === f
       f >=> returnFish === f
       \x -> f x >>= returnFish === f (definition of >=>)
       \x -> f x >>= return === f     (definition of returnFish)
       \x -> f x === f                (Monad law 1)
       f === f                        (eta reduction)
    -}

instance Monad m => MonadJoin m where
  returnJoin = return
  join x = x >>= id
    {- PROOF: join . returnJoin      ≡ id
       join . returnJoin === id
       returnJoin (join x) === id x    (definition of (.))
       returnJoin (x >>= id) === id x  (definition of join)
       return (x >>= id) === id x      (definition of returnJoin)
       id x === id x                   (Monad law 2)
    -}

instance MonadFish m => Monad m where
  return = returnFish
  (>>=) x f = (id >=> f) x
  {- PROOF: m >>= return    ≡ m
      m >>= return === m             (definition of )
      m >>= returnFish === m         (definition of return)
      (id >=> returnFish) m === m    (definition of >>=)
      (id >=> returnFish) m === id m (definition of id)
      id >=> returnFish === id       (eta reduction, MonadFish law 1)
  -}
