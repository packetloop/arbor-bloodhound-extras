{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Arbor.Bloodhound.V5Spec (spec) where

import Arbor.Bloodhound.V5
import Control.Exception
import Database.V5.Bloodhound

import qualified Data.Aeson as J (decode, encode, Value(..), toJSON)

import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}

throwESProtocolException :: a
throwESProtocolException = throw EsProtocolException
  { esProtoExBody = "\
      \{                                                                         \
      \  \"took\": 396,                                                          \
      \  \"errors\": true,                                                       \
      \  \"items\": [                                                            \
      \    {                                                                     \
      \      \"update\": {                                                       \
      \        \"_index\": \"vodka--attacks--2016-06\",                          \
      \        \"_type\": \"attack\",                                            \
      \        \"_id\": \"345107:93.91.200.219\",                                \
      \        \"status\": 500,                                                  \
      \        \"error\": {                                                      \
      \          \"type\": \"index_failed_engine_exception\",                    \
      \          \"reason\": \"Index failed for [attack#345107:93.91.200.219]\", \
      \          \"shard\": \"6\",                                               \
      \          \"index\": \"vodka--attacks--2016-06\",                         \
      \          \"caused_by\": {                                                \
      \            \"type\": \"already_closed_exception\",                       \
      \            \"reason\": \"translog is already closed\",                   \
      \            \"caused_by\": {                                              \
      \              \"type\": \"i_o_exception\",                                \
      \              \"reason\": \"i_o_exception: No space left on device\"      \
      \            }                                                             \
      \          }                                                               \
      \        }                                                                 \
      \      }                                                                   \
      \    },                                                                    \
      \    {                                                                     \
      \      \"update\": {                                                       \
      \        \"_index\": \"vodka--attacks--2012-09\",                          \
      \        \"_type\": \"attack\",                                            \
      \        \"_id\": \"110651:189.9.33.250\",                                 \
      \        \"_version\": 2,                                                  \
      \        \"_shards\": {                                                    \
      \          \"total\": 2,                                                   \
      \          \"successful\": 2,                                              \
      \          \"failed\": 0                                                   \
      \        },                                                                \
      \        \"status\": 200                                                   \
      \      }                                                                   \
      \    },                                                                    \
      \    {                                                                     \
      \      \"update\": {                                                       \
      \        \"_index\": \"vodka--attacks--2016-06\",                          \
      \        \"_type\": \"attack\",                                            \
      \        \"_id\": \"345107:93.91.200.219\",                                \
      \        \"status\": 500,                                                  \
      \        \"error\": {                                                      \
      \          \"type\": \"index_failed_engine_exception\",                    \
      \          \"reason\": \"Index failed for [attack#345107:93.91.200.219]\", \
      \          \"shard\": \"6\",                                               \
      \          \"index\": \"vodka--attacks--2016-06\",                         \
      \          \"caused_by\": {                                                \
      \            \"type\": \"already_closed_exception\",                       \
      \            \"reason\": \"translog is already closed\",                   \
      \            \"caused_by\": {                                              \
      \              \"type\": \"i_o_exception\",                                \
      \              \"reason\": \"i_o_exception: No space left on device\"      \
      \            }                                                             \
      \          }                                                               \
      \        }                                                                 \
      \      }                                                                   \
      \    },                                                                    \
      \    {                                                                     \
      \      \"update\": {                                                       \
      \        \"_index\": \"vodka--attacks--2012-09\",                          \
      \        \"_type\": \"attack\",                                            \
      \        \"_id\": \"110651:189.9.33.250\",                                 \
      \        \"_version\": 2,                                                  \
      \        \"_shards\": {                                                    \
      \          \"total\": 2,                                                   \
      \          \"successful\": 2,                                              \
      \          \"failed\": 0                                                   \
      \        },                                                                \
      \        \"status\": 200                                                   \
      \      }                                                                   \
      \    }                                                                     \
      \  ]                                                                       \
      \}                                                                         \
      \"
  }

spec :: Spec
spec = do
  describe "summariseEsProtocolException" $ do
    it "should filter out non-errors" $ do
      let expectedMessage :: Maybe J.Value = J.decode "\
        \ [                                                                   \
        \   {                                                                 \
        \     \"shard\": \"6\",                                               \
        \     \"caused_by\": {                                                \
        \       \"caused_by\": {                                              \
        \         \"reason\": \"i_o_exception: No space left on device\",     \
        \         \"type\": \"i_o_exception\"                                 \
        \       },                                                            \
        \       \"reason\": \"translog is already closed\",                   \
        \       \"type\": \"already_closed_exception\"                        \
        \     },                                                              \
        \     \"reason\": \"Index failed for [attack#345107:93.91.200.219]\", \
        \     \"type\": \"index_failed_engine_exception\",                    \
        \     \"index\": \"vodka--attacks--2016-06\"                          \
        \   },                                                                \
        \   {                                                                 \
        \     \"shard\": \"6\",                                               \
        \     \"caused_by\": {                                                \
        \       \"caused_by\": {                                              \
        \         \"reason\": \"i_o_exception: No space left on device\",     \
        \         \"type\": \"i_o_exception\"                                 \
        \       },                                                            \
        \       \"reason\": \"translog is already closed\",                   \
        \       \"type\": \"already_closed_exception\"                        \
        \     },                                                              \
        \     \"reason\": \"Index failed for [attack#345107:93.91.200.219]\", \
        \     \"type\": \"index_failed_engine_exception\",                    \
        \     \"index\": \"vodka--attacks--2016-06\"                          \
        \   }                                                                 \
        \ ]                                                                   \
        \"
      result <- throwESProtocolException
            `catch` summariseEsProtocolException
            `catch` \(e :: EsProtocolException) -> return (esProtoExBody e)

      J.decode result `shouldBe` expectedMessage
