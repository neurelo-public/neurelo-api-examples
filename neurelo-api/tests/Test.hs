{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Data.Typeable (Proxy(..))
import Test.Hspec
import Test.Hspec.QuickCheck

import PropMime
import Instances ()

import NeureloAPISpec.Model
import NeureloAPISpec.MimeTypes

main :: IO ()
main =
  hspec $ modifyMaxSize (const 10) $ do
    describe "JSON instances" $ do
      pure ()
      propMimeEq MimeJSON (Proxy :: Proxy AffectedRowsOutput)
      propMimeEq MimeJSON (Proxy :: Proxy AggregateByUser200Response)
      propMimeEq MimeJSON (Proxy :: Proxy AggregateUser)
      propMimeEq MimeJSON (Proxy :: Proxy CreateManyUser201Response)
      propMimeEq MimeJSON (Proxy :: Proxy CreateOneUser201Response)
      propMimeEq MimeJSON (Proxy :: Proxy ErrorResponse)
      propMimeEq MimeJSON (Proxy :: Proxy FindUser200Response)
      propMimeEq MimeJSON (Proxy :: Proxy FindUser400Response)
      propMimeEq MimeJSON (Proxy :: Proxy GroupByUser200Response)
      propMimeEq MimeJSON (Proxy :: Proxy NestedIntFilter)
      propMimeEq MimeJSON (Proxy :: Proxy NestedIntFilterNot)
      propMimeEq MimeJSON (Proxy :: Proxy NestedIntNullableFilter)
      propMimeEq MimeJSON (Proxy :: Proxy NestedIntNullableFilterNot)
      propMimeEq MimeJSON (Proxy :: Proxy NestedStringFilter)
      propMimeEq MimeJSON (Proxy :: Proxy NestedStringFilterNot)
      propMimeEq MimeJSON (Proxy :: Proxy NestedStringNullableFilter)
      propMimeEq MimeJSON (Proxy :: Proxy NestedStringNullableFilterNot)
      propMimeEq MimeJSON (Proxy :: Proxy NestedStringNullableWithAggregatesFilter)
      propMimeEq MimeJSON (Proxy :: Proxy NestedStringNullableWithAggregatesFilterNot)
      propMimeEq MimeJSON (Proxy :: Proxy NestedStringWithAggregatesFilter)
      propMimeEq MimeJSON (Proxy :: Proxy NestedStringWithAggregatesFilterNot)
      propMimeEq MimeJSON (Proxy :: Proxy NullableStringFieldUpdateOperationsInput)
      propMimeEq MimeJSON (Proxy :: Proxy QueryMode)
      propMimeEq MimeJSON (Proxy :: Proxy SortOrder)
      propMimeEq MimeJSON (Proxy :: Proxy StringFieldUpdateOperationsInput)
      propMimeEq MimeJSON (Proxy :: Proxy StringFilter)
      propMimeEq MimeJSON (Proxy :: Proxy StringNullableFilter)
      propMimeEq MimeJSON (Proxy :: Proxy StringNullableWithAggregatesFilter)
      propMimeEq MimeJSON (Proxy :: Proxy StringWithAggregatesFilter)
      propMimeEq MimeJSON (Proxy :: Proxy User)
      propMimeEq MimeJSON (Proxy :: Proxy UserAggregateInput)
      propMimeEq MimeJSON (Proxy :: Proxy UserCountAggregateOutputType)
      propMimeEq MimeJSON (Proxy :: Proxy UserCountOrderByAggregateInput)
      propMimeEq MimeJSON (Proxy :: Proxy UserCreateInput)
      propMimeEq MimeJSON (Proxy :: Proxy UserCreateManyInput)
      propMimeEq MimeJSON (Proxy :: Proxy UserGroupByInput)
      propMimeEq MimeJSON (Proxy :: Proxy UserGroupByOutputType)
      propMimeEq MimeJSON (Proxy :: Proxy UserMaxAggregateOutputType)
      propMimeEq MimeJSON (Proxy :: Proxy UserMaxOrderByAggregateInput)
      propMimeEq MimeJSON (Proxy :: Proxy UserMinAggregateOutputType)
      propMimeEq MimeJSON (Proxy :: Proxy UserMinOrderByAggregateInput)
      propMimeEq MimeJSON (Proxy :: Proxy UserOrderByWithAggregationInput)
      propMimeEq MimeJSON (Proxy :: Proxy UserOrderByWithRelationInput)
      propMimeEq MimeJSON (Proxy :: Proxy UserScalarFieldEnum)
      propMimeEq MimeJSON (Proxy :: Proxy UserScalarWhereWithAggregatesInput)
      propMimeEq MimeJSON (Proxy :: Proxy UserScalarWhereWithAggregatesInputAND)
      propMimeEq MimeJSON (Proxy :: Proxy UserScalarWhereWithAggregatesInputEmail)
      propMimeEq MimeJSON (Proxy :: Proxy UserScalarWhereWithAggregatesInputName)
      propMimeEq MimeJSON (Proxy :: Proxy UserSelectInput)
      propMimeEq MimeJSON (Proxy :: Proxy UserUpdateInput)
      propMimeEq MimeJSON (Proxy :: Proxy UserUpdateInputEmail)
      propMimeEq MimeJSON (Proxy :: Proxy UserUpdateInputName)
      propMimeEq MimeJSON (Proxy :: Proxy UserUpdateManyInput)
      propMimeEq MimeJSON (Proxy :: Proxy UserWhereInput)
      propMimeEq MimeJSON (Proxy :: Proxy UserWhereInputAND)
      propMimeEq MimeJSON (Proxy :: Proxy UserWhereInputEmail)
      propMimeEq MimeJSON (Proxy :: Proxy UserWhereInputName)
      propMimeEq MimeJSON (Proxy :: Proxy UserWhereUniqueInput)
      
