{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}

module Data.Type.Nat.Quote
  ( qN
  ) where

import Data.Type.Quote
import Data.Type.Nat
import Type.Family.Nat
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Control.Monad ((>=>))

qN :: QuasiQuoter
qN = QuasiQuoter
  { quoteExp  = parseAsNatTerm qq varE [|Z_|]  $ \x -> [|S_ $x|]
  , quotePat  = parseAsNatTerm qq varP [p|Z_|] $ \x -> [p|S_ $x|]
  , quoteType = parseAsNatTerm qq varT [t|Z|] $ \x -> [t|S $x|]
  , quoteDec  = stub qq "quoteDec"
  }
  where
  qq = "qN"

