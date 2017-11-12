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
       returnJoin (join x) === id x
       returnFish (join x) === id x
       returnFish ((id >=> id) x) === id x

       join . returnFish === id >=> returnFish
       (id >=> id) . returnFish
    -}


instance (Functor m, MonadJoin m) => Monad     m where
    return = returnJoin
    (>>=) x f = join $ fmap f x

instance (Functor m, MonadJoin m) => MonadFish m where
    returnFish = returnJoin
    (>=>) f g = \x -> join (fmap g (join (fmap f (returnJoin x))))

