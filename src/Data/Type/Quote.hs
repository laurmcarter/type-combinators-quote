{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Data.Type.Quote where

import Control.Arrow
import Language.Haskell.TH
import qualified Data.List as L
import Data.Maybe (maybeToList)
import Type.Family.Nat
import Text.Read (readMaybe)
import Data.Char (isSpace)
import qualified Language.Haskell.Meta.Parse as P
import Control.Monad ((>=>),(<=<))

failFrom :: Monad m => String -> String -> m a
failFrom src msg = fail $ src ++ ": " ++ msg

stub :: String -> String -> a -> Q b
stub qq fn _ = failFrom qq $ fn ++ " not provided"

-- Parsers {{{

parseExp :: String -> String -> Q Exp
parseExp qq = eitherM qq . P.parseExp

parsePat :: String -> String -> Q Pat
parsePat qq = eitherM qq . P.parsePat

parseType :: String -> String -> Q Type
parseType qq = eitherM qq . P.parseType

parseDecs :: String -> String -> Q [Dec]
parseDecs qq = eitherM qq . P.parseDecs

-- }}}

-- Nat Util {{{

parseN :: Monad m => String -> String -> m N
parseN qq = maybe
  (failFrom qq "couldn't parse number")
  (notNeg qq)
  . readMaybe

notNeg :: Monad m => String -> Int -> m N
notNeg qq n = maybe
  (failFrom qq $ "negative number: " ++ show n)
  return
  $ fromInt n

-- }}}

eitherM :: Monad m => String -> Either String a -> m a
eitherM qq = either (failFrom qq) return

maybeM :: Monad m => String -> String -> Maybe a -> m a
maybeM qq msg = maybe (failFrom qq msg) return

-- List Util {{{

readMaybeList :: Read a => String -> [a]
readMaybeList = maybeToList . readMaybe

commaSep :: String -> [String]
commaSep = map (strip isSpace) . breakOnAll ","

strip :: (a -> Bool) -> [a] -> [a]
strip pr = L.dropWhileEnd pr . dropWhile pr

breakOnAll :: Eq a => [a] -> [a] -> [[a]]
breakOnAll b as = b1 : case rest of
  [] -> []
  _  -> breakOnAll b rest
  where
  (b1,rest) = breakOn b as

breakOn :: Eq a => [a] -> [a] -> ([a],[a])
breakOn b = \case
  []           -> ([],[])
  as@(a : as') -> case getPrefix b as of
    Just sf -> ([],sf)
    _       -> first (a:) $ breakOn b as'

getPrefix :: Eq a => [a] -> [a] -> Maybe [a]
getPrefix p as
  | L.isPrefixOf p as = Just $ drop (length p) as
  | otherwise          = Nothing

-- }}}

-- AddTerm {{{

data AddTerm a
  = Simple a
  | Add String a
  deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

instance (a ~ Int) => Read (AddTerm a) where
  readsPrec d s0 =
    [ (Add x i,s3)
    | (x,s1)   <- lex s0
    , ("+",s2) <- lex s1
    , (i,s3)   <- reads s2
    ] ++
    [ (Simple i,s1)
    | (i,s1)   <- reads s0
    ]

parseAddTerm :: Monad m => String -> String -> m (AddTerm N)
parseAddTerm qq = traverse (notNeg qq)
  <=< maybeM qq "couldn't parse AddTerm"
    . readMaybe

fromAddTerm :: b -> (String -> b) -> (b -> a -> Q c) -> AddTerm a -> Q c
fromAddTerm simp var f = \case
  Simple a -> f simp    a
  Add x  a -> f (var x) a

parseAsNatTerm :: forall a. String
                  -> (Name -> Q a)
                  -> Q a -> (Q a -> Q a)
                  -> String -> Q a
parseAsNatTerm qq v z s = parseAddTerm qq >=> fromAddTerm z (v . mkName) go
  where
  go :: Q a -> N -> Q a
  go x = \case
    Z   -> x
    S n -> s $ go x n

-- }}}

