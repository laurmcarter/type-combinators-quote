{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Data.Type.Product.Quote
  ( qP
  ) where

import Data.Type.Quote
import Data.Type.Product
import Language.Haskell.TH
import Language.Haskell.TH.Quote

qP :: QuasiQuoter
qP = QuasiQuoter
  { quoteExp  = parseProd (parseExp qq) prodExp
  , quotePat  = parseProd (parsePat qq) prodPat
  , quoteType = stub qq "quoteType not provided"
  , quoteDec  = stub qq "quoteDec not provided"
  }
  where
  qq = "qP"

parseProd :: (String -> Q a) -> ([Q a] -> Q a) -> String -> Q a
parseProd prs bld = bld . map prs . commaSep

prodExp :: [Q Exp] -> Q Exp
prodExp = \case
  e : es -> [| $e :< $(prodExp es) |]
  _      -> [| Ø |]

prodPat :: [Q Pat] -> Q Pat
prodPat = \case
  e : es -> [p| $e :< $(prodPat es) |]
  _      -> [p| Ø |]

