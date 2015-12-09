
module Data.Type.Product.Quote where

import Data.Type.Product
import Type.Family.List
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta.Parse
import Text.Read

π :: QuasiQuoter
π = QuasiQuoter
  { quoteExp  = undefined
  , quotePat  = undefined
  , quoteType = undefined
  , quoteDec  = undefined
  }

{-
parseProdExp :: String -> Q Exp
parseProdExp s0 = 
-}

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn 

