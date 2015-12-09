{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module Data.Type.Sym.Quote
  ( qS
  ) where

import Data.Type.Quote
import Data.Type.Sym
import Language.Haskell.TH
import Language.Haskell.TH.Quote

qS :: QuasiQuoter
qS = QuasiQuoter
  { quoteExp  = parseSymExp
  , quotePat  = stub qq "quotePat"
  , quoteType = parseSymType
  , quoteDec  = stub qq "quoteDec"
  }
  where
  qq = "qS"

parseSymExp :: String -> Q Exp
parseSymExp s = [| Sym :: $(parseSymType s) |]

parseSymType :: String -> Q Type
parseSymType s = [t| Sym $(litT $ strTyLit s) |]

