#!/usr/bin/env stack
-- stack script --resolver lts-22.11 --package http-conduit,aeson

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Network.HTTP.Simple
import Data.Aeson
import GHC.Generics

data User = User
  { email :: String
  , name :: String
  , id :: String
  } deriving (Show, Generic)

instance FromJSON User
instance ToJSON User

data CreateUserInput = CreateUserInput
  { email :: String
  , password :: String
  , name :: String
  } deriving (Show, Generic)

instance FromJSON CreateUserInput
instance ToJSON CreateUserInput

data UpdateUserInput = UpdateUserInput
  { email :: String
  , password :: String
  , newName :: String
  , newEmail :: String
  } deriving (Show, Generic)

instance FromJSON UpdateUserInput
instance ToJSON UpdateUserInput

createUser :: CreateUserInput -> IO User
createUser input = do
  request' <- parseRequest "POST http://localhost:12345/user"
  let request = setRequestBodyJSON input request'
  response <- httpJSON request
  return $ getResponseBody response

listUsers :: String -> IO [User]
listUsers search = do
  request <- parseRequest $ "GET http://localhost:12345/user/list?s=" ++ search
  response <- httpJSON request
  return $ getResponseBody response

updateUser :: UpdateUserInput -> IO User
updateUser input = do
  request' <- parseRequest "PUT http://localhost:12345/user/update"
  let request = setRequestBodyJSON input request'
  response <- httpJSON request
  return $ getResponseBody response

main :: IO ()
main = do
    putStrLn "Enter email:"
    email <- getLine
    putStrLn "Enter password:"
    password <- getLine
    putStrLn "Enter name:"
    name <- getLine

    let createUserInput = CreateUserInput email password name
    user <- createUser createUserInput
    putStrLn "Created user:"
    print user

    let domain = dropWhile (/= '@') email
    putStrLn "Users with the same domain:"
    users <- listUsers domain
    print users

    putStrLn "Enter new email:"
    newEmail <- getLine
    putStrLn "Enter new name:"
    newName <- getLine

    let updateUserInput = UpdateUserInput email password newName newEmail
    updatedUser <- updateUser updateUserInput
    putStrLn "Updated user:"
    print updatedUser