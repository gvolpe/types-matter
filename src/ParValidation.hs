{-# LANGUAGE DataKinds, OverloadedStrings #-}

module ParValidation
  ( validationProgram
  )
where

import           Control.Arrow                  ( left )
import           Control.Parallel
import           Control.Parallel.Class
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Refined
import           Refined.Instances              ( )

type Eff a = Either [Text] a

type Name = Refined NonEmpty Text
type Age = Refined (GreaterThan 17) Int

data Person = Person
  { personAge :: Age
  , personName :: Name
  } deriving Show

ref :: Predicate p x => x -> Eff (Refined p x)
ref x = left (\e -> [pack $ show e]) (refine x)

validationProgram :: IO ()
validationProgram =
  let person = parMapN (ref 10 :: Eff Age) (ref "" :: Eff Name) Person
  in  print (show person)
