{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}

module BlockThreePartTwo where

import           Prelude (Applicative (..), Foldable (..), Functor (..),
                          Monoid (..), Traversable (..), undefined, ($))

newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
    fmap f a = Identity (f $ runIdentity a)

instance Applicative Identity where
    pure = Identity
    (<*>) f a = Identity ((runIdentity f) (runIdentity a))

instance Foldable Identity where
    foldr f acc idn = f (runIdentity idn) acc

instance Traversable Identity where
    traverse f idn = fmap Identity (f (runIdentity idn))

--

data Either a b = Left a | Right b

instance Functor (Either a) where
    fmap _ (Left a)  = Left a
    fmap f (Right b) = Right (f b)

instance Applicative (Either a) where
    pure = Right
    (<*>) _ (Left x)          = Left x
    (<*>) (Left f) _          = Left f
    (<*>) (Right f) (Right y) = Right (f y)

instance Foldable (Either a) where
    foldr f acc (Left x)  = acc
    foldr f acc (Right x) = f x acc

instance Traversable (Either a) where
    traverse f (Left x)  = pure (Left x)
    traverse f (Right x) = fmap Right (f x)

--

data Tree a = Leaf | Node a (Tree a) (Tree a)

instance Functor Tree where
    fmap _ Leaf           = Leaf
    fmap f (Node val l r) = Node (f val) (fmap f l) (fmap f r)

instance Applicative Tree where
    pure x = Node x (pure x) (pure x)
    (<*>) (Leaf) y                    = Leaf
    (<*>) (Node f l r) Leaf           = Leaf
    (<*>) (Node f l r) (Node val x y) = Node (f val) (l <*> x) (r <*> y)

instance Foldable Tree where
    foldr f acc Leaf         = acc
    foldr f acc (Node x l r) = foldr f (f x (foldr f acc r)) l

instance Traversable Tree where
    traverse f Leaf         = pure Leaf
    traverse f (Node x l r) = undefined

--

newtype Const a b = Const { getConst :: a }

instance Functor (Const a) where
    fmap _ (Const c) = Const c

instance Monoid a => Applicative (Const a) where
    pure x = Const mempty
    (<*>) (Const f) (Const c) = Const $ mappend f c

instance Foldable (Const a) where
    foldr f acc (Const c) = acc

instance Traversable (Const a) where
    traverse f (Const c) = pure $ Const c

--

data Pair a b = Pair a b

instance Functor (Pair a) where
    fmap f (Pair a b) = Pair a (f b)

instance Monoid a => Applicative (Pair a) where
    pure x = Pair mempty x
    (<*>) (Pair _ f) (Pair x y) = Pair mempty (f y)

instance Foldable (Pair a) where
    foldr f acc (Pair _ b) = f b acc

instance Traversable (Pair a) where
    traverse f p@(Pair a b) = fmap (Pair a) (f b)

