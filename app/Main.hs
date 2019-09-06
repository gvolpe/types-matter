{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Main where

import           Data.Maybe                     ( fromMaybe )
import           Http
import           Refined
import           Stringy

stringProgram :: IO ()
stringProgram = do
  putStrLn $ showName "gvolpe@github.com" "12345" "foo bar"
  putStrLn $ showName' (Username "hey@gmail.com") (Name "123") (Email "#123")
  putStrLn $ showName' username name email
  putStrLn $ showNameRefined username' name' email'
 where
  username  = fromMaybe (error "Invalid username") (mkUsername "gvolpe")
  name      = fromMaybe (error "Invalid name") (mkName "Gabriel")
  email     = fromMaybe (error "Invalid email") (mkEmail "gvolpe@gmail.com")
  -- email    = fromMaybe (error "Invalid email") (mkEmail "123") -- runtime error
  username' = $$(refineTH "gvolpe") :: Username'
  name'     = $$(refineTH "Gabriel") :: Name'
  email'    = $$(refineTH "gvolpe@github.com") :: Email'

uriProgram :: IO ()
uriProgram = do
  print $ mkUri host port
  print $ mkUri' host' port'
 where
  host  = fromMaybe (error "Invalid host") (mkHttpHost "127.0.0.1")
  port  = fromMaybe (error "Invaild port") (mkHttpPort 8080)
  host' = $$(refineTH "localhost") :: HttpHost'
  port' = $$(refineTH 8080) :: HttpPort'

main :: IO ()
main = stringProgram
