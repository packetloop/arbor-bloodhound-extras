{-# LANGUAGE OverloadedStrings #-}

module Arbor.Bloodhound.V5
    ( summariseEsProtocolException
    ) where

import Control.Exception
import Control.Lens
import Database.V5.Bloodhound
import Data.Aeson.Lens

import qualified Data.Aeson.Lens as J
import qualified Data.Aeson as J (decode, encode, Value(..), toJSON)

-- | Rewrite the body of the exception to only contain errors.
summariseEsProtocolException :: EsProtocolException -> a
summariseEsProtocolException e = throw EsProtocolException
  { esProtoExBody = errorsMessage
  }
  where text          = esProtoExBody e
        responses     = J.decode text :: Maybe J.Value
        errors        = responses ^..  _Just . J.key "items" . _Array . each . J.key "update" . J.key "error"
        errorsMessage = if null errors then text else J.encode (J.toJSON errors)
