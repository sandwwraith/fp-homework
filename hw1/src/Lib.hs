module Lib where

import           Data.Foldable  (Foldable (..))
import           Data.List      (sort, splitAt)
import           Data.Monoid    (Sum (..))
import           Data.Semigroup (Semigroup (..))
import           System.Random  (newStdGen, randomRs)
import           TreePrinters   (Tree (..), directoryPrint, verticalPrint)

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen

-- Block 1

order3 :: Ord x => (x, x, x) -> (x, x, x)
order3 (a, b, c) = let [x, y, z] = sort [a, b, c] in (x, y, z)

highestBit :: Int -> Int
highestBit = fst . highestBitHard

highestBitHard :: Int -> (Int, Int)
highestBitHard x = let exp = floor $ logBase 2.0 (fromIntegral x) in (2 ^ exp, exp)

smartReplicate :: [Int] -> [Int]
smartReplicate = concatMap (\x -> replicate x x)

contains :: Eq a => a -> [[a]] -> [[a]]
contains item = filter (elem item)

-- Block 2

removeAt :: Int -> [a] -> [a]
removeAt idx xs = snd $ removeAtHard idx xs

removeAtHard :: Int -> [a] -> (Maybe a, [a])
removeAtHard idx xs = wrap $ splitAt idx xs
    where
      wrap (xs, [])   = (Nothing, xs)
      wrap (xs, y:ys) = (Just y, xs ++ ys)

collectEvery :: Int -> [a] -> ([a], [a])
collectEvery n xs = wrap $ splitAt (n-1) xs
    where
      wrap (xs, []) = (xs, [])
      wrap (xs, y:ys) = let (a, b) = collectEvery n ys
                        in (xs ++ a, y:b)

stringSum :: String -> Int
stringSum str = sum (map read (words str))

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = let (a, b) = splitAt (length xs `div` 2) xs
               in merge (mergeSort a) (mergeSort b)
    where
      merge xs [] = xs
      merge [] xs = xs
      merge (x:xs) (y:ys)
        | x <= y    = x:merge xs (y:ys)
        | otherwise = y:merge (x:xs) ys

-- Block 3

data WeekDay = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Show, Enum, Eq)

nextDay :: WeekDay -> WeekDay
nextDay Sun = Mon
nextDay day = succ day

afterDays :: WeekDay -> Int -> WeekDay
afterDays a 0 = a
afterDays a n = nextDay $ afterDays a (n-1)

isWeekend :: WeekDay -> Bool
isWeekend day | day == Sat || day == Sun = True
              | otherwise = False

daysToParty :: WeekDay -> Int
daysToParty = countDays 0
    where
      countDays acc Fri = acc
      countDays acc d   = countDays (acc+1) (nextDay d)

data Monster = Monster
    { monsterAttack :: Int
    , monsterHealth :: Int
    }

data Knight = Knight
    { knightAttack :: Int
    , knightHealth :: Int
    }

-- Returns (Knight wins, number of rounds)
battle :: Knight -> Monster -> (Bool, Int)
battle k m = round k m 0
    where
      round :: Knight -> Monster -> Int -> (Bool, Int)
      round (Knight ka 0) _ acc = (False, acc)
      round _ (Monster ma 0) acc = (True, acc)
      round (Knight ka kh) (Monster ma mh) acc
            | even acc = round (Knight ka kh) (Monster ma (max (mh-ka) 0)) $ acc + 1
            | otherwise = round (Knight ka (max (kh-ma) 0)) (Monster ma mh) $ acc + 1

class Fightable a where
    attack :: a -> Int
    receiveDmg :: a -> Int -> a
    isDead :: a -> Bool

instance Fightable Monster where
    attack (Monster atk _) = atk
    receiveDmg (Monster atk hp) dmg = Monster atk (max (hp - dmg) 0)
    isDead (Monster _ hp) = hp == 0

instance Fightable Knight where
    attack (Knight atk _) = atk
    receiveDmg (Knight atk hp) dmg = Knight atk (max (hp - dmg) 0)
    isDead (Knight _ hp) = hp == 0

battleHard :: (Fightable a, Fightable b) => a -> b -> (Bool, Int)
battleHard m1 m2 = round m1 m2 0
    where
      round m1 m2 acc
        | isDead m1 = (False, acc)
        | isDead m2 = (True, acc)
        | even acc = round m1 (receiveDmg m2 (attack m1)) $ acc + 1
        | otherwise = round (receiveDmg m1 (attack m2)) m2 $ acc + 1

data Vector a = Vector2D a a | Vector3D a a a deriving Show

vToList :: Floating a => Vector a -> [a]
vToList (Vector2D x y)   = [x, y, 0]
vToList (Vector3D x y z) = [x, y, z]

negateVec :: Floating a => Vector a -> Vector a
negateVec (Vector2D x y)   = Vector2D (-x) (-y)
negateVec (Vector3D x y z) = Vector3D (-x) (-y) (-z)

len :: Floating a => Vector a -> a
len vec = sqrt $ sum $ map (^2) (vToList vec)

sumVec :: Floating a => Vector a -> Vector a -> Vector a
sumVec (Vector2D x y) (Vector2D x1 y1) = Vector2D (x + x1) (y + y1)
sumVec vec1 vec2 = fromList3 $ zipWith (+) (vToList vec1) (vToList vec2)
    where
      fromList3 (x:y:z:_) = Vector3D x y z

dotMul :: Floating a => Vector a -> Vector a -> a
dotMul vec1 vec2 = sqrt $ sum $ zipWith (*) (vToList vec1) (vToList vec2)

distance :: Floating a => Vector a -> Vector a -> a
distance vec1 vec2 = len $ sumVec vec1 (negateVec vec2)

crossMul :: Floating a => Vector a -> Vector a -> Vector a
crossMul vec1 vec2 = mulList (vToList vec1) (vToList vec2)
    where
      mulList (x1:y1:z1:_) (x2:y2:z2:_) = Vector3D (y1*z2 - z1*y2) (z1*x2 - x1*z2) (x1*y2 - y1*x2)

data Nat = Z | S Nat deriving Show

natToInteger :: Nat -> Int
natToInteger Z     = 0
natToInteger (S n) = 1 + natToInteger(n)

instance Num Nat where
    n + Z = n
    n + (S m) = S (n + m)

    _ * Z = Z
    n * (S m) = n + (n * m)

    fromInteger 0 = Z
    fromInteger x = S (fromInteger (x-1))

    n - Z = n
    (S n) - (S m) = n - m

instance Eq Nat where
    Z == Z = True
    (S n) == (S m) = n == m
    _ == _ = False

instance Ord Nat where
    Z <= _ = True
    (S n) <= (S m) = n <= m
    (S _) <= Z = False

-- Block 3

-- defined in TreePrinters
-- data Tree a = Leaf | Node a (Tree a) (Tree a)

isEmpty :: Tree a -> Bool
isEmpty Leaf = True
isEmpty _    = False

size :: Tree a -> Int
size Leaf                = 0
size (Node _ left right) = (size left) + (size right) + 1

findTree :: (Ord a) => Tree a -> a -> Maybe (Tree a)
findTree Leaf _  = Nothing
findTree node@(Node x left right) i
    | x == i = Just node
    | i < x = findTree left i
    | i > x = findTree right i

insert :: (Ord a) => Tree a -> a -> Tree a
insert Leaf i = Node i Leaf Leaf
insert node@(Node x left right) i
    | x == i = node
    | i < x = Node x (insert left i) right
    | i > x = Node x left (insert right i)

fromList :: (Ord a) => [a] -> Tree a
fromList [] = Leaf
fromList xs = foldl' insert Leaf xs

-- Block 4

instance Foldable Tree where
    foldMap _ Leaf         = mempty
    foldMap f (Node x l r) = foldMap f l `mappend` f x `mappend` foldMap f r

    foldr f acc Leaf         = acc
    foldr f acc (Node x l r) = foldr f (f x (foldr f acc r)) l


splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x xs = foldr f [[]] xs
    where
      f item (y:ys)
        | item == x = []:y:ys
        | otherwise = (item:y):ys

joinWith :: Eq a => a -> [[a]] -> [a]
joinWith _ [[]] = []
joinWith _ [x] = x
joinWith i (x:xs) = foldl' f x (xs)
    where
      f item acc = item ++ [i] ++ acc

-- Block 5

maybeConcat :: [Maybe ([a])] -> [a]
maybeConcat xs = foldr f [] xs
    where
      f Nothing acc   = acc
      f (Just li) acc = li ++ acc

eitherConcat :: (Monoid l, Monoid r) => [Either l r] -> (l,r)
eitherConcat xs = foldr f (mempty, mempty) xs
    where
      f (Left l)  (ls, rs) = (l `mappend` ls, rs)
      f (Right r) (ls, rs) = (ls, r `mappend` rs)

data NonEmpty a = a :| [a]

instance Semigroup (NonEmpty t) where
    (x :| xs) <> (y :| ys) = x :| (xs ++ [y] ++ ys)

newtype Identity a = Identity { runIdentity :: a }

instance Semigroup t => Semigroup (Identity t) where
    (Identity a) <> (Identity b) = Identity (a <> b)

instance Monoid t => Monoid (Identity t) where
    mempty = mempty
    mappend (Identity a) (Identity b) = Identity (a `mappend` b)

newtype Name = Name String deriving Show

instance Semigroup Name where
    (Name a) <> (Name b) = Name $ a ++ "." ++ b

instance Monoid Name where
    mempty = Name ""
    mappend (Name a) (Name b)
      | a == "" = Name b
      | b == "" = Name a
      | otherwise = Name $ a ++ "." ++ b

newtype Endo a = Endo { getEndo :: a -> a }

instance (Semigroup a) => Semigroup (Endo a) where
    (Endo a) <> (Endo b) = Endo (a . b)

instance Monoid (Endo a) where
    mempty = Endo id
    mappend (Endo a) (Endo b) = Endo (a . b)

newtype Arrow a b = Arrow { getArrow :: a -> b }

instance Semigroup b => Semigroup (Arrow a b) where
    (Arrow f1) <> (Arrow f2) = Arrow (\a -> (f1 a) <> (f2 a))

instance Monoid    b => Monoid    (Arrow a b) where
    mempty = Arrow (\_ -> mempty)
    mappend (Arrow a) (Arrow b) = Arrow (\x -> (a x) `mappend` (b x))

instance Ord a => Semigroup (Tree a) where
    node <> Leaf = node
    node <> (Node y ly ry) = (insert node y) <> ly <> ry

instance Ord a => Monoid (Tree a) where
    mempty = Leaf

    node `mappend` Leaf = node
    node `mappend` (Node y ly ry) = ((insert node y) `mappend` ly) `mappend` ry
