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

module Data.Type.Length.Quote
  ( qL
  ) where

import Data.Type.Length
import Data.Type.Quote
import Language.Haskell.TH
import Language.Haskell.TH.Quote

qL :: QuasiQuoter
qL = QuasiQuoter
  { quoteExp  = parseAsNatTerm qq varE [|LZ|]  $ \x -> [|LS $x|]
  , quotePat  = parseAsNatTerm qq varP [p|LZ|] $ \x -> [p|LS $x|]
  , quoteType = stub   qq "quoteType"
  , quoteDec  = stub   qq "quoteDec"
  }
  where
  qq = "qL"

