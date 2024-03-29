{-
   Neurelo API Spec

   No description provided (generated by Openapi Generator https://github.com/openapitools/openapi-generator)

   OpenAPI Version: 3.0.3
   Neurelo API Spec API version: 0
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : NeureloAPISpec.API.User
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module NeureloAPISpec.API.User where

import NeureloAPISpec.Core
import NeureloAPISpec.MimeTypes
import NeureloAPISpec.Model as M

import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Data as P (Typeable, TypeRep, typeOf, typeRep)
import qualified Data.Foldable as P
import qualified Data.Map as Map
import qualified Data.Maybe as P
import qualified Data.Proxy as P (Proxy(..))
import qualified Data.Set as Set
import qualified Data.String as P
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Time as TI
import qualified Network.HTTP.Client.MultipartFormData as NH
import qualified Network.HTTP.Media as ME
import qualified Network.HTTP.Types as NH
import qualified Web.FormUrlEncoded as WH
import qualified Web.HttpApiData as WH

import Data.Text (Text)
import GHC.Base ((<|>))

import Prelude ((==),(/=),($), (.),(<$>),(<*>),(>>=),Maybe(..),Bool(..),Char,Double,FilePath,Float,Int,Integer,String,fmap,undefined,mempty,maybe,pure,Monad,Applicative,Functor)
import qualified Prelude as P

-- * Operations


-- ** User

-- *** aggregateByUser

-- | @GET \/rest\/user\/__aggregate@
-- 
-- Aggregate by user
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
aggregateByUser
  :: SelectUserAggregateInput -- ^ "select"
  -> NeureloAPISpecRequest AggregateByUser MimeNoContent AggregateByUser200Response MimeJSON
aggregateByUser (SelectUserAggregateInput select) =
  _mkRequest "GET" ["/rest/user/__aggregate"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `addQuery` toJsonQuery ("select", Just select)

data AggregateByUser  
instance HasOptionalParam AggregateByUser Filter where
  applyOptionalParam req (Filter xs) =
    req `addQuery` toJsonQuery ("filter", Just xs)
instance HasOptionalParam AggregateByUser OrderBy where
  applyOptionalParam req (OrderBy xs) =
    req `addQuery` toJsonQueryColl CommaSeparated ("order_by", Just xs)
instance HasOptionalParam AggregateByUser Skip where
  applyOptionalParam req (Skip xs) =
    req `addQuery` toQuery ("skip", Just xs)
instance HasOptionalParam AggregateByUser Take where
  applyOptionalParam req (Take xs) =
    req `addQuery` toQuery ("take", Just xs)
-- | @application/json@
instance Produces AggregateByUser MimeJSON


-- *** createManyUser

-- | @POST \/rest\/user@
-- 
-- Create multiple user records
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
createManyUser
  :: (Consumes CreateManyUser MimeJSON, MimeRender MimeJSON UserCreateManyInput2)
  => UserCreateManyInput2 -- ^ "userCreateManyInput"
  -> NeureloAPISpecRequest CreateManyUser MimeJSON CreateManyUser201Response MimeJSON
createManyUser userCreateManyInput =
  _mkRequest "POST" ["/rest/user"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `setBodyParam` userCreateManyInput

data CreateManyUser 
instance HasBodyParam CreateManyUser UserCreateManyInput2 

-- | @application/json@
instance Consumes CreateManyUser MimeJSON

-- | @application/json@
instance Produces CreateManyUser MimeJSON


-- *** createOneUser

-- | @POST \/rest\/user\/__one@
-- 
-- Create one user record
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
createOneUser
  :: (Consumes CreateOneUser MimeJSON, MimeRender MimeJSON UserCreateInput)
  => UserCreateInput -- ^ "userCreateInput"
  -> NeureloAPISpecRequest CreateOneUser MimeJSON CreateOneUser201Response MimeJSON
createOneUser userCreateInput =
  _mkRequest "POST" ["/rest/user/__one"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `setBodyParam` userCreateInput

data CreateOneUser 
instance HasBodyParam CreateOneUser UserCreateInput 
instance HasOptionalParam CreateOneUser Select where
  applyOptionalParam req (Select xs) =
    req `addQuery` toJsonQuery ("select", Just xs)

-- | @application/json@
instance Consumes CreateOneUser MimeJSON

-- | @application/json@
instance Produces CreateOneUser MimeJSON


-- *** deleteUser

-- | @DELETE \/rest\/user@
-- 
-- Delete multiple user records
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
deleteUser
  :: NeureloAPISpecRequest DeleteUser MimeNoContent CreateManyUser201Response MimeJSON
deleteUser =
  _mkRequest "DELETE" ["/rest/user"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)

data DeleteUser  
instance HasOptionalParam DeleteUser Filter where
  applyOptionalParam req (Filter xs) =
    req `addQuery` toJsonQuery ("filter", Just xs)
-- | @application/json@
instance Produces DeleteUser MimeJSON


-- *** deleteUserByEmail

-- | @DELETE \/rest\/user\/email\/{value}@
-- 
-- Delete one user record by email
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
deleteUserByEmail
  :: Value -- ^ "value"
  -> NeureloAPISpecRequest DeleteUserByEmail MimeNoContent CreateOneUser201Response MimeJSON
deleteUserByEmail (Value value) =
  _mkRequest "DELETE" ["/rest/user/email/",toPath value]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)

data DeleteUserByEmail  
instance HasOptionalParam DeleteUserByEmail Select where
  applyOptionalParam req (Select xs) =
    req `addQuery` toJsonQuery ("select", Just xs)
-- | @application/json@
instance Produces DeleteUserByEmail MimeJSON


-- *** deleteUserById

-- | @DELETE \/rest\/user\/{value}@
-- 
-- Delete one user record by id
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
deleteUserById
  :: Value -- ^ "value"
  -> NeureloAPISpecRequest DeleteUserById MimeNoContent CreateOneUser201Response MimeJSON
deleteUserById (Value value) =
  _mkRequest "DELETE" ["/rest/user/",toPath value]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)

data DeleteUserById  
instance HasOptionalParam DeleteUserById Select where
  applyOptionalParam req (Select xs) =
    req `addQuery` toJsonQuery ("select", Just xs)
-- | @application/json@
instance Produces DeleteUserById MimeJSON


-- *** findUser

-- | @GET \/rest\/user@
-- 
-- Retrieve multiple user records
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
findUser
  :: NeureloAPISpecRequest FindUser MimeNoContent FindUser200Response MimeJSON
findUser =
  _mkRequest "GET" ["/rest/user"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)

data FindUser  
instance HasOptionalParam FindUser Select where
  applyOptionalParam req (Select xs) =
    req `addQuery` toJsonQuery ("select", Just xs)
instance HasOptionalParam FindUser Filter where
  applyOptionalParam req (Filter xs) =
    req `addQuery` toJsonQuery ("filter", Just xs)
instance HasOptionalParam FindUser OrderBy where
  applyOptionalParam req (OrderBy xs) =
    req `addQuery` toJsonQueryColl CommaSeparated ("order_by", Just xs)
instance HasOptionalParam FindUser Skip where
  applyOptionalParam req (Skip xs) =
    req `addQuery` toQuery ("skip", Just xs)
instance HasOptionalParam FindUser Take where
  applyOptionalParam req (Take xs) =
    req `addQuery` toQuery ("take", Just xs)
-- | @application/json@
instance Produces FindUser MimeJSON


-- *** findUserByEmail

-- | @GET \/rest\/user\/email\/{value}@
-- 
-- Find one user record by email
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
findUserByEmail
  :: Value -- ^ "value"
  -> NeureloAPISpecRequest FindUserByEmail MimeNoContent CreateOneUser201Response MimeJSON
findUserByEmail (Value value) =
  _mkRequest "GET" ["/rest/user/email/",toPath value]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)

data FindUserByEmail  
instance HasOptionalParam FindUserByEmail Select where
  applyOptionalParam req (Select xs) =
    req `addQuery` toJsonQuery ("select", Just xs)
-- | @application/json@
instance Produces FindUserByEmail MimeJSON


-- *** findUserById

-- | @GET \/rest\/user\/{value}@
-- 
-- Find one user record by id
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
findUserById
  :: Value -- ^ "value"
  -> NeureloAPISpecRequest FindUserById MimeNoContent CreateOneUser201Response MimeJSON
findUserById (Value value) =
  _mkRequest "GET" ["/rest/user/",toPath value]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)

data FindUserById  
instance HasOptionalParam FindUserById Select where
  applyOptionalParam req (Select xs) =
    req `addQuery` toJsonQuery ("select", Just xs)
-- | @application/json@
instance Produces FindUserById MimeJSON


-- *** groupByUser

-- | @GET \/rest\/user\/__groupBy@
-- 
-- Group by user
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
groupByUser
  :: SelectUserGroupByInput -- ^ "select"
  -> NeureloAPISpecRequest GroupByUser MimeNoContent GroupByUser200Response MimeJSON
groupByUser (SelectUserGroupByInput select) =
  _mkRequest "GET" ["/rest/user/__groupBy"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `addQuery` toJsonQuery ("select", Just select)

data GroupByUser  
instance HasOptionalParam GroupByUser Filter where
  applyOptionalParam req (Filter xs) =
    req `addQuery` toJsonQuery ("filter", Just xs)
instance HasOptionalParam GroupByUser OrderByUserOrderByWithAggregationInput where
  applyOptionalParam req (OrderByUserOrderByWithAggregationInput xs) =
    req `addQuery` toJsonQueryColl CommaSeparated ("order_by", Just xs)
instance HasOptionalParam GroupByUser GroupBy where
  applyOptionalParam req (GroupBy xs) =
    req `addQuery` toJsonQueryColl CommaSeparated ("group_by", Just xs)
instance HasOptionalParam GroupByUser Having where
  applyOptionalParam req (Having xs) =
    req `addQuery` toJsonQuery ("having", Just xs)
instance HasOptionalParam GroupByUser Skip where
  applyOptionalParam req (Skip xs) =
    req `addQuery` toQuery ("skip", Just xs)
instance HasOptionalParam GroupByUser Take where
  applyOptionalParam req (Take xs) =
    req `addQuery` toQuery ("take", Just xs)
-- | @application/json@
instance Produces GroupByUser MimeJSON


-- *** updateUser

-- | @PATCH \/rest\/user@
-- 
-- Update multiple user records
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
updateUser
  :: (Consumes UpdateUser MimeJSON, MimeRender MimeJSON UserUpdateManyInput)
  => UserUpdateManyInput -- ^ "userUpdateManyInput"
  -> NeureloAPISpecRequest UpdateUser MimeJSON CreateManyUser201Response MimeJSON
updateUser userUpdateManyInput =
  _mkRequest "PATCH" ["/rest/user"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `setBodyParam` userUpdateManyInput

data UpdateUser 
instance HasBodyParam UpdateUser UserUpdateManyInput 
instance HasOptionalParam UpdateUser Filter where
  applyOptionalParam req (Filter xs) =
    req `addQuery` toJsonQuery ("filter", Just xs)

-- | @application/json@
instance Consumes UpdateUser MimeJSON

-- | @application/json@
instance Produces UpdateUser MimeJSON


-- *** updateUserByEmail

-- | @PATCH \/rest\/user\/email\/{value}@
-- 
-- Update one user record by email
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
updateUserByEmail
  :: (Consumes UpdateUserByEmail MimeJSON, MimeRender MimeJSON UserUpdateInput)
  => UserUpdateInput -- ^ "userUpdateInput"
  -> Value -- ^ "value"
  -> NeureloAPISpecRequest UpdateUserByEmail MimeJSON CreateOneUser201Response MimeJSON
updateUserByEmail userUpdateInput (Value value) =
  _mkRequest "PATCH" ["/rest/user/email/",toPath value]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `setBodyParam` userUpdateInput

data UpdateUserByEmail 
instance HasBodyParam UpdateUserByEmail UserUpdateInput 
instance HasOptionalParam UpdateUserByEmail Select where
  applyOptionalParam req (Select xs) =
    req `addQuery` toJsonQuery ("select", Just xs)

-- | @application/json@
instance Consumes UpdateUserByEmail MimeJSON

-- | @application/json@
instance Produces UpdateUserByEmail MimeJSON


-- *** updateUserById

-- | @PATCH \/rest\/user\/{value}@
-- 
-- Update one user record by id
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
updateUserById
  :: (Consumes UpdateUserById MimeJSON, MimeRender MimeJSON UserUpdateInput)
  => UserUpdateInput -- ^ "userUpdateInput"
  -> Value -- ^ "value"
  -> NeureloAPISpecRequest UpdateUserById MimeJSON CreateOneUser201Response MimeJSON
updateUserById userUpdateInput (Value value) =
  _mkRequest "PATCH" ["/rest/user/",toPath value]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `setBodyParam` userUpdateInput

data UpdateUserById 
instance HasBodyParam UpdateUserById UserUpdateInput 
instance HasOptionalParam UpdateUserById Select where
  applyOptionalParam req (Select xs) =
    req `addQuery` toJsonQuery ("select", Just xs)

-- | @application/json@
instance Consumes UpdateUserById MimeJSON

-- | @application/json@
instance Produces UpdateUserById MimeJSON

