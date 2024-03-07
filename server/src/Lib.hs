{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Lib
  ( main,
  )
where

import Configuration.Dotenv qualified as Dotenv
import Data.Aeson
import Data.ByteString.Lazy.Char8 qualified as BSLC
import Data.Maybe
import Data.Password.PBKDF2 qualified as PBKDF2
import Data.Text qualified as T
import Network.HTTP.Client qualified as NH
import Network.HTTP.Client.TLS (newTlsManager)
import NeureloAPISpec.API as API
import NeureloAPISpec.Client as API
import NeureloAPISpec.Core as API
import NeureloAPISpec.MimeTypes as API
import NeureloAPISpec.Model as API
import System.Environment
import Yesod

data App = App
  { apiConfig :: NeureloAPISpecConfig,
    apiHttpManager :: NH.Manager
  }

runNeureloApi ::
  (Produces req accept, MimeUnrender accept res, MimeType contentType) =>
  NeureloAPISpecRequest req contentType res accept ->
  HandlerFor App res
runNeureloApi call = do
  app <- getYesod
  liftIO $ either (fail . mimeError) return =<< API.dispatchMime' (apiHttpManager app) (apiConfig app) call

mkYesod
  "App"
  [parseRoutes|
  /user/list ListUsers GET
  /user CreateUser POST
  /user/update UpdateUser PUT
|]

instance Yesod App

defaultSelectFilter =
  Select
    mkUserSelectInput
      { userSelectInputEmail = Just True,
        userSelectInputId = Just True,
        userSelectInputName = Just True
      }

getListUsers = do
  let findCallRaw =
        API.findUser
          -&- Take 10 -- lets not return everything
          -&- defaultSelectFilter
  mSearchString <- lookupGetParam "s"
  let findCall =
        case mSearchString of
          Nothing -> findCallRaw
          Just searchString ->
            findCallRaw
              -&- API.Filter
                mkUserWhereInput
                  { userWhereInputEmail =
                      Just
                        mkUserWhereInputEmail
                          { userWhereInputEmailContains = Just searchString
                          }
                  }
  res <- runNeureloApi findCall
  return $ toJSON $ findUser200ResponseData res

data CreateUserInput = CreateUserInput
  { createUserEmail :: T.Text,
    createUserName :: T.Text,
    createUserPassword :: PBKDF2.Password
  }

instance FromJSON CreateUserInput where
  parseJSON = withObject "CreateUserInput" $ \o -> do
    email <- o .: "email"
    name <- o .: "name"
    password <- o .: "password"
    return
      CreateUserInput
        { createUserEmail = email,
          createUserName = name,
          createUserPassword = PBKDF2.mkPassword password
        }

postCreateUser = do
  createUser :: CreateUserInput <- requireInsecureJsonBody
  PBKDF2.PasswordHash passwordHash <- PBKDF2.hashPassword (createUserPassword createUser)
  res <-
    runNeureloApi $
      API.createOneUser $
        (mkUserCreateInput (createUserEmail createUser) passwordHash)
          { userCreateInputName = Just (createUserName createUser)
          }
  return $ toJSON $ createOneUser201ResponseData res

data UpdateUserInput = UpdateUserInput
  { updateUserEmail :: T.Text,
    updateUserPassword :: PBKDF2.Password,
    updateUserNewEmail :: Maybe T.Text,
    updateUserNewName :: Maybe T.Text
  }

instance FromJSON UpdateUserInput where
  parseJSON = withObject "UpdateUserInput" $ \o -> do
    email <- o .: "email"
    newEmail <- o .:? "newEmail"
    newName <- o .:? "newName"
    password <- o .: "password"
    return
      UpdateUserInput
        { updateUserEmail = email,
          updateUserNewName = newName,
          updateUserNewEmail = newEmail,
          updateUserPassword = PBKDF2.mkPassword password
        }

putUpdateUser = do
  updateUser :: UpdateUserInput <- requireInsecureJsonBody
  existingUsers <-
    fmap findUser200ResponseData $
      runNeureloApi $
        findUser
          -&- API.Filter
            mkUserWhereInput
              { userWhereInputEmail =
                  Just
                    mkUserWhereInputEmail
                      { userWhereInputEmailEq = Just (updateUserEmail updateUser)
                      }
              }
  existingUser <-
    case existingUsers of
      [] -> notFound
      existingUser : _ -> return existingUser
  let passwordHash = PBKDF2.PasswordHash (fromJust (userPasswordhash existingUser))
  case PBKDF2.checkPassword (updateUserPassword updateUser) passwordHash of
    PBKDF2.PasswordCheckFail -> notAuthenticated
    PBKDF2.PasswordCheckSuccess ->
      fmap (toJSON . createOneUser201ResponseData) $
        runNeureloApi $
          updateUserById
            ( mkUserUpdateInput
                { userUpdateInputEmail =
                    fmap (\email -> mkUserUpdateInputEmail {userUpdateInputEmailSet = Just email}) (updateUserNewEmail updateUser),
                  userUpdateInputName =
                    fmap (\name -> mkUserUpdateInputName {userUpdateInputNameSet = Just name}) (updateUserNewName updateUser)
                }
            )
            (API.Value (fromJust (userId existingUser)))

main :: IO ()
main = do
  Dotenv.loadFile Dotenv.defaultConfig
  token <- getEnv "NEURELO_API_KEY"
  url <- getEnv "NEURELO_API_BASE_PATH"
  config0 <- API.withStderrLogging =<< API.newConfig
  let config =
        config0
          { API.configHost = BSLC.pack url,
            API.configAuthMethods = [AnyAuthMethod (AuthApiKeyApiKey (T.pack token))]
          }
  mgr <- newTlsManager
  warp 12345 (App config mgr)
