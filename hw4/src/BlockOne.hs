{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module BlockOne where
import           Data.List           (elemIndex)
import qualified Data.Text           as T
import           Language.Haskell.TH

someFunc :: String
someFunc = "someFunc"

fstN :: Int -> Q Exp
fstN n = do
   x <- newName "x"
   pure $ LamE [TupP $ VarP x : replicate (n - 1) WildP] (VarE x)

choseByIndices :: Int -> [Int] -> Q Exp
choseByIndices n ind = do
    let locals = map (mkName . ("local" ++) . show) [0..n-1]
    pure $ LamE [TupP $ map (ctor locals) [0..n-1]] (TupE (map (helper locals) ind))
      where
        helper :: [Name] -> Int -> Exp
        helper locals i = VarE $ locals !! i
        ctor locals i = case elemIndex i ind of
            Nothing -> WildP
            Just _  -> VarP $ locals !! i

class ShowText a where
    showText :: a -> T.Text

genShowText :: Name -> Q [Dec]
genShowText name = do
    TyConI (DataD _ _ _ _ [RecC _ fields] _) <- reify name

    let names = map (\(n,_,_) -> n) fields

    let showField :: Name -> Q Exp
        showField name' = [|\x -> T.pack $ s ++ " = " ++ show ($(varE name') x)|]
          where s = nameBase name

    let showFields :: Q Exp
        showFields = listE $ map showField names

    let className = nameBase name

    [d|instance ShowText $(conT name) where
            showText x = className `T.append` " {" `T.append` T.intercalate ", "
                    (map ($ x) $showFields) `T.append` "}" |]
