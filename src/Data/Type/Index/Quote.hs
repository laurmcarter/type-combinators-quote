{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module Data.Type.Index.Quote
  ( qI
  ) where

import Data.Type.Quote
import Data.Type.Index
import Language.Haskell.TH
import Language.Haskell.TH.Quote

qI :: QuasiQuoter
qI = QuasiQuoter
  { quoteExp  = parseAsNatTerm qq varE [|IZ|]  $ \x -> [|IS $x|]
  , quotePat  = parseAsNatTerm qq varP [p|IZ|] $ \x -> [p|IS $x|]
  , quoteType = stub qq "quoteType"
  , quoteDec  = stub qq "quoteDec"
  }
  where
  qq = "qI"

