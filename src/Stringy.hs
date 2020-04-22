{-# LANGUAGE DataKinds, OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Stringy where

import           Control.Monad                  ( unless )
import           Data.Typeable                  ( typeOf )
import           Refined

showName :: String -> String -> String -> String
showName username name email =
  "Hi "
    <> name
    <> "! "
    <> "Your username is "
    <> username
    <> " and your email is "
    <> email

newtype Username = Username String
newtype Name = Name String
newtype Email = Email String

showName' :: Username -> Name -> Email -> String
showName' (Username u) (Name n) (Email e) =
  "Hi " <> n <> "! " <> "Your username is " <> u <> " and your email is " <> e

-- Smart constructors --

mkUsername :: String -> Maybe Username
mkUsername [] = Nothing
mkUsername u  = Just (Username u)

mkName :: String -> Maybe Name
mkName [] = Nothing
mkName n  = Just (Name n)

-- Let's pretend we validate it properly
mkEmail :: String -> Maybe Email
mkEmail e = if '@' `elem` e then Just (Email e) else Nothing

-- Refinement types --

data EmailPred

instance Predicate EmailPred String where
  validate p value = unless ('@' `elem` value)
    $ throwRefineOtherException (typeOf p) "Invalid email"

type Username' = Refined NonEmpty String
type Name' = Refined NonEmpty String
type Email' = Refined EmailPred String

showNameRefined :: Username' -> Name' -> Email' -> String
showNameRefined username name email =
  "Hi "
    <> unrefine name
    <> "! "
    <> "Your username is "
    <> unrefine username
    <> " and your email is "
    <> unrefine email
