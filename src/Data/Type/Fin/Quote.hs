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

module Data.Type.Fin.Quote
  ( qF
  ) where

import Data.Type.Quote
import Data.Type.Fin
import Language.Haskell.TH
import Language.Haskell.TH.Quote

qF :: QuasiQuoter
qF = QuasiQuoter
  { quoteExp  = parseAsNatTerm qq varE [|FZ|]  $ \x -> [|FS $x|]
  , quotePat  = parseAsNatTerm qq varP [p|FZ|] $ \x -> [p|FS $x|]
  , quoteType = stub qq "quoteType"
  , quoteDec  = stub qq "quoteDec"
  }
  where
  qq = "qF"

