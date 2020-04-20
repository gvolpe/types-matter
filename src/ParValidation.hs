{-# LANGUAGE DataKinds, OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module ParValidation
  ( validationProgram
  )
where

import           Control.Arrow                  ( left )
import           Control.Parallel
import           Control.Parallel.Class
import           Data.Foldable                  ( traverse_ )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.Validation                ( fromEither
                                                , toEither
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

makePerson :: Int -> Text -> Eff Person
makePerson a n = parMapN (ref a) (ref n) Person

validationProgram :: IO ()
validationProgram = case makePerson 0 "" of
  (Left  e) -> traverse_ print e
  (Right p) -> print p

-------------- Refinement types -------------------

me :: Person
me = Person $$(refineTH 32) $$(refineTH "Gabriel")

-------------- Sequential Validation -------------

mkPersonSeq :: Int -> Text -> Either RefineException Person
mkPersonSeq a n = do
  age  <- refine a
  name <- refine n
  return $ Person age name

-------------- Without Parallel -------------------

mkPerson :: Int -> Text -> Eff Person
mkPerson a n = toEither $ Person <$> fromEither (ref a) <*> fromEither (ref n)

-------------- Zipping Lists ----------------------

n1 :: [Int]
n1 = [1 .. 5]

n2 :: [Int]
n2 = [6 .. 10]

n3 :: [Int]
n3 = (+) <$> n1 <*> n2

n4 :: [Int]
n4 = parMapN n1 n2 (+)
--n4 = getZipList $ (+) <$> ZipList n1 <*> ZipList n2
