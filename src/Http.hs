{-# LANGUAGE DataKinds, OverloadedStrings, RecordWildCards #-}

module Http where

import           Data.Text                      ( Text
                                                , null
                                                , pack
                                                , unpack
                                                )
import           Prelude                 hiding ( null )
import           Refined

newtype HttpHost = HttpHost { unHost :: String } deriving Show
newtype HttpPort = HttpPort { unPort :: Int } deriving Show
newtype HttpUri = HttpUri { unUri :: Text } deriving Show

mkHttpHost :: String -> Maybe HttpHost
mkHttpHost [] = Nothing
mkHttpHost h  = Just (HttpHost h)

-- Validation happens at runtime
mkHttpPort :: Int -> Maybe HttpPort
mkHttpPort n = if n >= 1024 && n <= 49151 then Just (HttpPort n) else Nothing

-- We assume the inputs are validated
mkUri :: HttpHost -> HttpPort -> HttpUri
mkUri HttpHost {..} HttpPort {..} =
  HttpUri (pack $ unHost <> ":" <> show unPort)

-- Refinement types --

-- Validation happens at compile time
type HttpHost' = Refined NonEmpty String
-- https://en.wikipedia.org/wiki/List_of_TCP_and_UDP_port_numbers
type HttpPort' = Refined (FromTo 1024 49151) Int
-- Refined (And (Not (LessThan 1024)) (Not (GreaterThan 49151))) Int

mkUri' :: HttpHost' -> HttpPort' -> HttpUri
mkUri' host port =
  HttpUri (pack $ unrefine host <> ":" <> show (unrefine port))
