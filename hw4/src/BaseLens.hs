{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE Rank2Types     #-}

module BaseLens where

import           Data.Functor.Const    (Const (..), getConst)
import           Data.Functor.Identity (Identity (..), runIdentity)

type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t
type Lens' s a  = Lens s s a a

set :: Lens' s a -> a -> s -> s
set l b = runIdentity . l (\_ -> Identity b)

over :: Lens' s a -> (a -> a) -> s -> s
over l f = runIdentity . l (Identity . f)

view :: Lens' s a -> s -> a
view l s = getConst (l Const s)

(.~) :: Lens' s a -> a -> s -> s
(.~) = set

(^.) :: s -> Lens' s a -> a
s ^. l = view l s

(%~) :: Lens' s a -> (a -> a) -> s -> s
(%~) = over

-- _1 :: Functor f => (a -> f b) -> (a, x) -> f (b, x)
_1 :: Lens (a, x) (b, x) a b
_1 f (a, x) = (\b -> (b, x)) <$> f a

-- _2 :: Functor f => (a -> f b) -> (x, a) -> f (x, b)
_2 :: Lens (x, a) (x, b) a b
_2 f (x, b) = (\a -> (x, a)) <$> f b
