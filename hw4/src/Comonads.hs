{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Comonads where

import           Control.Comonad        (Comonad (..), extract)

import           Control.Comonad.Traced (Traced, runTraced, traced)
import           Data.Text

data Renew s e a = Renew (e -> a) s

class Monoid e => MonoidAction s e where
    act :: s -> e -> s

instance                     Functor (Renew s e) where
    -- fmap :: (a -> b) -> (Renew s e a) -> (Renew s e b)
    fmap f (Renew g s) = Renew (f . g) s
instance MonoidAction s e => Comonad (Renew s e) where
    -- extract :: Renew s e a -> a
    extract (Renew g _) = g mempty
    -- duplicate :: Renew s e a -> Renew s e (Renew s e a)
    duplicate (Renew g s) = Renew (\x -> Renew g (act s x)) s
    -- extend :: (Renew s e a -> b) -> Renew s e a -> Renew s e b
    extend f = fmap f . duplicate

data Project = Project
    { projectName :: Text
    , hasBenchs   :: Bool
    , hasGithub   :: Bool
    , hasTravis   :: Bool
    } deriving (Show, Eq)

data ProjectSettings = ProjectSettings
    { settingsBenchs :: Bool  -- ^ enable benchmarks for project?
    , settingsGithub :: Bool  -- ^ set up github     for project?
    , settingsTravis :: Bool  -- ^ set up Travis CI  for project?
    }

instance Monoid ProjectSettings where
    mempty = ProjectSettings False False False
    (ProjectSettings b1 g1 t1) `mappend` (ProjectSettings b2 g2 t2) =
        ProjectSettings (b1 || b2) (g1 || g2) (t1 || t2)

type ProjectBuilder = Traced ProjectSettings Project

travis :: ProjectBuilder -> Project
travis builder = let (Project n b g _) = runTraced builder mempty
    in Project n b g g

github :: ProjectBuilder -> Project
github b = runTraced b (ProjectSettings False True False)

benchs :: ProjectBuilder -> Project
benchs b = runTraced b (ProjectSettings True False False)

buildProject :: Text -> ProjectBuilder
buildProject name = traced (\(ProjectSettings b g t) -> Project name b g t)
