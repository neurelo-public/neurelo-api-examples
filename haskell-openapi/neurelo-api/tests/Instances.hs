{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-matches #-}

module Instances where

import NeureloAPISpec.Model
import NeureloAPISpec.Core

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Time as TI
import qualified Data.Vector as V
import Data.String (fromString)

import Control.Monad
import Data.Char (isSpace)
import Data.List (sort)
import Test.QuickCheck

import ApproxEq

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary TI.Day where
  arbitrary = TI.ModifiedJulianDay . (2000 +) <$> arbitrary
  shrink = (TI.ModifiedJulianDay <$>) . shrink . TI.toModifiedJulianDay

instance Arbitrary TI.UTCTime where
  arbitrary =
    TI.UTCTime <$> arbitrary <*> (TI.secondsToDiffTime <$> choose (0, 86401))

instance Arbitrary BL.ByteString where
    arbitrary = BL.pack <$> arbitrary
    shrink xs = BL.pack <$> shrink (BL.unpack xs)

instance Arbitrary ByteArray where
    arbitrary = ByteArray <$> arbitrary
    shrink (ByteArray xs) = ByteArray <$> shrink xs

instance Arbitrary Binary where
    arbitrary = Binary <$> arbitrary
    shrink (Binary xs) = Binary <$> shrink xs

instance Arbitrary DateTime where
    arbitrary = DateTime <$> arbitrary
    shrink (DateTime xs) = DateTime <$> shrink xs

instance Arbitrary Date where
    arbitrary = Date <$> arbitrary
    shrink (Date xs) = Date <$> shrink xs

#if MIN_VERSION_aeson(2,0,0)
#else
-- | A naive Arbitrary instance for A.Value:
instance Arbitrary A.Value where
  arbitrary = arbitraryValue
#endif

arbitraryValue :: Gen A.Value
arbitraryValue =
  frequency [(3, simpleTypes), (1, arrayTypes), (1, objectTypes)]
    where
      simpleTypes :: Gen A.Value
      simpleTypes =
        frequency
          [ (1, return A.Null)
          , (2, liftM A.Bool (arbitrary :: Gen Bool))
          , (2, liftM (A.Number . fromIntegral) (arbitrary :: Gen Int))
          , (2, liftM (A.String . T.pack) (arbitrary :: Gen String))
          ]
      mapF (k, v) = (fromString k, v)
      simpleAndArrays = frequency [(1, sized sizedArray), (4, simpleTypes)]
      arrayTypes = sized sizedArray
      objectTypes = sized sizedObject
      sizedArray n = liftM (A.Array . V.fromList) $ replicateM n simpleTypes
      sizedObject n =
        liftM (A.object . map mapF) $
        replicateM n $ (,) <$> (arbitrary :: Gen String) <*> simpleAndArrays

-- | Checks if a given list has no duplicates in _O(n log n)_.
hasNoDups
  :: (Ord a)
  => [a] -> Bool
hasNoDups = go Set.empty
  where
    go _ [] = True
    go s (x:xs)
      | s' <- Set.insert x s
      , Set.size s' > Set.size s = go s' xs
      | otherwise = False

instance ApproxEq TI.Day where
  (=~) = (==)

arbitraryReduced :: Arbitrary a => Int -> Gen a
arbitraryReduced n = resize (n `div` 2) arbitrary

arbitraryReducedMaybe :: Arbitrary a => Int -> Gen (Maybe a)
arbitraryReducedMaybe 0 = elements [Nothing]
arbitraryReducedMaybe n = arbitraryReduced n

arbitraryReducedMaybeValue :: Int -> Gen (Maybe A.Value)
arbitraryReducedMaybeValue 0 = elements [Nothing]
arbitraryReducedMaybeValue n = do
  generated <- arbitraryReduced n
  if generated == Just A.Null
    then return Nothing
    else return generated

-- * Models

instance Arbitrary AffectedRowsOutput where
  arbitrary = sized genAffectedRowsOutput

genAffectedRowsOutput :: Int -> Gen AffectedRowsOutput
genAffectedRowsOutput n =
  AffectedRowsOutput
    <$> arbitraryReducedMaybe n -- affectedRowsOutputCount :: Maybe Int
  
instance Arbitrary AggregateByUser200Response where
  arbitrary = sized genAggregateByUser200Response

genAggregateByUser200Response :: Int -> Gen AggregateByUser200Response
genAggregateByUser200Response n =
  AggregateByUser200Response
    <$> arbitraryReduced n -- aggregateByUser200ResponseData :: AggregateUser
  
instance Arbitrary AggregateUser where
  arbitrary = sized genAggregateUser

genAggregateUser :: Int -> Gen AggregateUser
genAggregateUser n =
  AggregateUser
    <$> arbitraryReducedMaybe n -- aggregateUserCount :: Maybe UserCountAggregateOutputType
    <*> arbitraryReducedMaybe n -- aggregateUserMax :: Maybe UserMaxAggregateOutputType
    <*> arbitraryReducedMaybe n -- aggregateUserMin :: Maybe UserMinAggregateOutputType
  
instance Arbitrary CreateManyUser201Response where
  arbitrary = sized genCreateManyUser201Response

genCreateManyUser201Response :: Int -> Gen CreateManyUser201Response
genCreateManyUser201Response n =
  CreateManyUser201Response
    <$> arbitraryReduced n -- createManyUser201ResponseData :: AffectedRowsOutput
  
instance Arbitrary CreateOneUser201Response where
  arbitrary = sized genCreateOneUser201Response

genCreateOneUser201Response :: Int -> Gen CreateOneUser201Response
genCreateOneUser201Response n =
  CreateOneUser201Response
    <$> arbitraryReduced n -- createOneUser201ResponseData :: User
  
instance Arbitrary ErrorResponse where
  arbitrary = sized genErrorResponse

genErrorResponse :: Int -> Gen ErrorResponse
genErrorResponse n =
  ErrorResponse
    <$> arbitraryReducedMaybeValue n -- errorResponseDetails :: Maybe A.Value
    <*> arbitrary -- errorResponseError :: Text
  
instance Arbitrary FindUser200Response where
  arbitrary = sized genFindUser200Response

genFindUser200Response :: Int -> Gen FindUser200Response
genFindUser200Response n =
  FindUser200Response
    <$> arbitraryReduced n -- findUser200ResponseData :: [User]
  
instance Arbitrary FindUser400Response where
  arbitrary = sized genFindUser400Response

genFindUser400Response :: Int -> Gen FindUser400Response
genFindUser400Response n =
  FindUser400Response
    <$> arbitraryReduced n -- findUser400ResponseErrors :: [ErrorResponse]
  
instance Arbitrary GroupByUser200Response where
  arbitrary = sized genGroupByUser200Response

genGroupByUser200Response :: Int -> Gen GroupByUser200Response
genGroupByUser200Response n =
  GroupByUser200Response
    <$> arbitraryReduced n -- groupByUser200ResponseData :: [UserGroupByOutputType]
  
instance Arbitrary NestedIntFilter where
  arbitrary = sized genNestedIntFilter

genNestedIntFilter :: Int -> Gen NestedIntFilter
genNestedIntFilter n =
  NestedIntFilter
    <$> arbitraryReducedMaybe n -- nestedIntFilterEq :: Maybe Int
    <*> arbitraryReducedMaybe n -- nestedIntFilterEquals :: Maybe Int
    <*> arbitraryReducedMaybe n -- nestedIntFilterGt :: Maybe Int
    <*> arbitraryReducedMaybe n -- nestedIntFilterGte :: Maybe Int
    <*> arbitraryReducedMaybe n -- nestedIntFilterIn :: Maybe [Int]
    <*> arbitraryReducedMaybe n -- nestedIntFilterLt :: Maybe Int
    <*> arbitraryReducedMaybe n -- nestedIntFilterLte :: Maybe Int
    <*> arbitraryReducedMaybe n -- nestedIntFilterNot :: Maybe NestedIntFilterNot
    <*> arbitraryReducedMaybe n -- nestedIntFilterNotIn :: Maybe [Int]
  
instance Arbitrary NestedIntFilterNot where
  arbitrary = sized genNestedIntFilterNot

genNestedIntFilterNot :: Int -> Gen NestedIntFilterNot
genNestedIntFilterNot n =
  NestedIntFilterNot
    <$> arbitraryReducedMaybe n -- nestedIntFilterNotEq :: Maybe Int
    <*> arbitraryReducedMaybe n -- nestedIntFilterNotEquals :: Maybe Int
    <*> arbitraryReducedMaybe n -- nestedIntFilterNotGt :: Maybe Int
    <*> arbitraryReducedMaybe n -- nestedIntFilterNotGte :: Maybe Int
    <*> arbitraryReducedMaybe n -- nestedIntFilterNotIn :: Maybe [Int]
    <*> arbitraryReducedMaybe n -- nestedIntFilterNotLt :: Maybe Int
    <*> arbitraryReducedMaybe n -- nestedIntFilterNotLte :: Maybe Int
    <*> arbitraryReducedMaybe n -- nestedIntFilterNotNot :: Maybe NestedIntFilterNot
    <*> arbitraryReducedMaybe n -- nestedIntFilterNotNotIn :: Maybe [Int]
  
instance Arbitrary NestedIntNullableFilter where
  arbitrary = sized genNestedIntNullableFilter

genNestedIntNullableFilter :: Int -> Gen NestedIntNullableFilter
genNestedIntNullableFilter n =
  NestedIntNullableFilter
    <$> arbitraryReducedMaybe n -- nestedIntNullableFilterEq :: Maybe Int
    <*> arbitraryReducedMaybe n -- nestedIntNullableFilterEquals :: Maybe Int
    <*> arbitraryReducedMaybe n -- nestedIntNullableFilterGt :: Maybe Int
    <*> arbitraryReducedMaybe n -- nestedIntNullableFilterGte :: Maybe Int
    <*> arbitraryReducedMaybe n -- nestedIntNullableFilterIn :: Maybe [Int]
    <*> arbitraryReducedMaybe n -- nestedIntNullableFilterIsSet :: Maybe Bool
    <*> arbitraryReducedMaybe n -- nestedIntNullableFilterLt :: Maybe Int
    <*> arbitraryReducedMaybe n -- nestedIntNullableFilterLte :: Maybe Int
    <*> arbitraryReducedMaybe n -- nestedIntNullableFilterNot :: Maybe NestedIntNullableFilterNot
    <*> arbitraryReducedMaybe n -- nestedIntNullableFilterNotIn :: Maybe [Int]
  
instance Arbitrary NestedIntNullableFilterNot where
  arbitrary = sized genNestedIntNullableFilterNot

genNestedIntNullableFilterNot :: Int -> Gen NestedIntNullableFilterNot
genNestedIntNullableFilterNot n =
  NestedIntNullableFilterNot
    <$> arbitraryReducedMaybe n -- nestedIntNullableFilterNotEq :: Maybe Int
    <*> arbitraryReducedMaybe n -- nestedIntNullableFilterNotEquals :: Maybe Int
    <*> arbitraryReducedMaybe n -- nestedIntNullableFilterNotGt :: Maybe Int
    <*> arbitraryReducedMaybe n -- nestedIntNullableFilterNotGte :: Maybe Int
    <*> arbitraryReducedMaybe n -- nestedIntNullableFilterNotIn :: Maybe [Int]
    <*> arbitraryReducedMaybe n -- nestedIntNullableFilterNotIsSet :: Maybe Bool
    <*> arbitraryReducedMaybe n -- nestedIntNullableFilterNotLt :: Maybe Int
    <*> arbitraryReducedMaybe n -- nestedIntNullableFilterNotLte :: Maybe Int
    <*> arbitraryReducedMaybe n -- nestedIntNullableFilterNotNot :: Maybe NestedIntNullableFilterNot
    <*> arbitraryReducedMaybe n -- nestedIntNullableFilterNotNotIn :: Maybe [Int]
  
instance Arbitrary NestedStringFilter where
  arbitrary = sized genNestedStringFilter

genNestedStringFilter :: Int -> Gen NestedStringFilter
genNestedStringFilter n =
  NestedStringFilter
    <$> arbitraryReducedMaybe n -- nestedStringFilterContains :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringFilterEndsWith :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringFilterEq :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringFilterEquals :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringFilterGt :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringFilterGte :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringFilterIn :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- nestedStringFilterLt :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringFilterLte :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringFilterNot :: Maybe NestedStringFilterNot
    <*> arbitraryReducedMaybe n -- nestedStringFilterNotIn :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- nestedStringFilterStartsWith :: Maybe Text
  
instance Arbitrary NestedStringFilterNot where
  arbitrary = sized genNestedStringFilterNot

genNestedStringFilterNot :: Int -> Gen NestedStringFilterNot
genNestedStringFilterNot n =
  NestedStringFilterNot
    <$> arbitraryReducedMaybe n -- nestedStringFilterNotContains :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringFilterNotEndsWith :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringFilterNotEq :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringFilterNotEquals :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringFilterNotGt :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringFilterNotGte :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringFilterNotIn :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- nestedStringFilterNotLt :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringFilterNotLte :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringFilterNotNot :: Maybe NestedStringFilterNot
    <*> arbitraryReducedMaybe n -- nestedStringFilterNotNotIn :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- nestedStringFilterNotStartsWith :: Maybe Text
  
instance Arbitrary NestedStringNullableFilter where
  arbitrary = sized genNestedStringNullableFilter

genNestedStringNullableFilter :: Int -> Gen NestedStringNullableFilter
genNestedStringNullableFilter n =
  NestedStringNullableFilter
    <$> arbitraryReducedMaybe n -- nestedStringNullableFilterContains :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringNullableFilterEndsWith :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringNullableFilterEq :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringNullableFilterEquals :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringNullableFilterGt :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringNullableFilterGte :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringNullableFilterIn :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- nestedStringNullableFilterIsSet :: Maybe Bool
    <*> arbitraryReducedMaybe n -- nestedStringNullableFilterLt :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringNullableFilterLte :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringNullableFilterNot :: Maybe NestedStringNullableFilterNot
    <*> arbitraryReducedMaybe n -- nestedStringNullableFilterNotIn :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- nestedStringNullableFilterStartsWith :: Maybe Text
  
instance Arbitrary NestedStringNullableFilterNot where
  arbitrary = sized genNestedStringNullableFilterNot

genNestedStringNullableFilterNot :: Int -> Gen NestedStringNullableFilterNot
genNestedStringNullableFilterNot n =
  NestedStringNullableFilterNot
    <$> arbitraryReducedMaybe n -- nestedStringNullableFilterNotContains :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringNullableFilterNotEndsWith :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringNullableFilterNotEq :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringNullableFilterNotEquals :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringNullableFilterNotGt :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringNullableFilterNotGte :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringNullableFilterNotIn :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- nestedStringNullableFilterNotIsSet :: Maybe Bool
    <*> arbitraryReducedMaybe n -- nestedStringNullableFilterNotLt :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringNullableFilterNotLte :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringNullableFilterNotNot :: Maybe NestedStringNullableFilterNot
    <*> arbitraryReducedMaybe n -- nestedStringNullableFilterNotNotIn :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- nestedStringNullableFilterNotStartsWith :: Maybe Text
  
instance Arbitrary NestedStringNullableWithAggregatesFilter where
  arbitrary = sized genNestedStringNullableWithAggregatesFilter

genNestedStringNullableWithAggregatesFilter :: Int -> Gen NestedStringNullableWithAggregatesFilter
genNestedStringNullableWithAggregatesFilter n =
  NestedStringNullableWithAggregatesFilter
    <$> arbitraryReducedMaybe n -- nestedStringNullableWithAggregatesFilterCount :: Maybe NestedIntNullableFilter
    <*> arbitraryReducedMaybe n -- nestedStringNullableWithAggregatesFilterMax :: Maybe NestedStringNullableFilter
    <*> arbitraryReducedMaybe n -- nestedStringNullableWithAggregatesFilterMin :: Maybe NestedStringNullableFilter
    <*> arbitraryReducedMaybe n -- nestedStringNullableWithAggregatesFilterContains :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringNullableWithAggregatesFilterEndsWith :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringNullableWithAggregatesFilterEq :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringNullableWithAggregatesFilterEquals :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringNullableWithAggregatesFilterGt :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringNullableWithAggregatesFilterGte :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringNullableWithAggregatesFilterIn :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- nestedStringNullableWithAggregatesFilterIsSet :: Maybe Bool
    <*> arbitraryReducedMaybe n -- nestedStringNullableWithAggregatesFilterLt :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringNullableWithAggregatesFilterLte :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringNullableWithAggregatesFilterNot :: Maybe NestedStringNullableWithAggregatesFilterNot
    <*> arbitraryReducedMaybe n -- nestedStringNullableWithAggregatesFilterNotIn :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- nestedStringNullableWithAggregatesFilterStartsWith :: Maybe Text
  
instance Arbitrary NestedStringNullableWithAggregatesFilterNot where
  arbitrary = sized genNestedStringNullableWithAggregatesFilterNot

genNestedStringNullableWithAggregatesFilterNot :: Int -> Gen NestedStringNullableWithAggregatesFilterNot
genNestedStringNullableWithAggregatesFilterNot n =
  NestedStringNullableWithAggregatesFilterNot
    <$> arbitraryReducedMaybe n -- nestedStringNullableWithAggregatesFilterNotCount :: Maybe NestedIntNullableFilter
    <*> arbitraryReducedMaybe n -- nestedStringNullableWithAggregatesFilterNotMax :: Maybe NestedStringNullableFilter
    <*> arbitraryReducedMaybe n -- nestedStringNullableWithAggregatesFilterNotMin :: Maybe NestedStringNullableFilter
    <*> arbitraryReducedMaybe n -- nestedStringNullableWithAggregatesFilterNotContains :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringNullableWithAggregatesFilterNotEndsWith :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringNullableWithAggregatesFilterNotEq :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringNullableWithAggregatesFilterNotEquals :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringNullableWithAggregatesFilterNotGt :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringNullableWithAggregatesFilterNotGte :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringNullableWithAggregatesFilterNotIn :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- nestedStringNullableWithAggregatesFilterNotIsSet :: Maybe Bool
    <*> arbitraryReducedMaybe n -- nestedStringNullableWithAggregatesFilterNotLt :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringNullableWithAggregatesFilterNotLte :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringNullableWithAggregatesFilterNotNot :: Maybe NestedStringNullableWithAggregatesFilterNot
    <*> arbitraryReducedMaybe n -- nestedStringNullableWithAggregatesFilterNotNotIn :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- nestedStringNullableWithAggregatesFilterNotStartsWith :: Maybe Text
  
instance Arbitrary NestedStringWithAggregatesFilter where
  arbitrary = sized genNestedStringWithAggregatesFilter

genNestedStringWithAggregatesFilter :: Int -> Gen NestedStringWithAggregatesFilter
genNestedStringWithAggregatesFilter n =
  NestedStringWithAggregatesFilter
    <$> arbitraryReducedMaybe n -- nestedStringWithAggregatesFilterCount :: Maybe NestedIntFilter
    <*> arbitraryReducedMaybe n -- nestedStringWithAggregatesFilterMax :: Maybe NestedStringFilter
    <*> arbitraryReducedMaybe n -- nestedStringWithAggregatesFilterMin :: Maybe NestedStringFilter
    <*> arbitraryReducedMaybe n -- nestedStringWithAggregatesFilterContains :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringWithAggregatesFilterEndsWith :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringWithAggregatesFilterEq :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringWithAggregatesFilterEquals :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringWithAggregatesFilterGt :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringWithAggregatesFilterGte :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringWithAggregatesFilterIn :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- nestedStringWithAggregatesFilterLt :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringWithAggregatesFilterLte :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringWithAggregatesFilterNot :: Maybe NestedStringWithAggregatesFilterNot
    <*> arbitraryReducedMaybe n -- nestedStringWithAggregatesFilterNotIn :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- nestedStringWithAggregatesFilterStartsWith :: Maybe Text
  
instance Arbitrary NestedStringWithAggregatesFilterNot where
  arbitrary = sized genNestedStringWithAggregatesFilterNot

genNestedStringWithAggregatesFilterNot :: Int -> Gen NestedStringWithAggregatesFilterNot
genNestedStringWithAggregatesFilterNot n =
  NestedStringWithAggregatesFilterNot
    <$> arbitraryReducedMaybe n -- nestedStringWithAggregatesFilterNotCount :: Maybe NestedIntFilter
    <*> arbitraryReducedMaybe n -- nestedStringWithAggregatesFilterNotMax :: Maybe NestedStringFilter
    <*> arbitraryReducedMaybe n -- nestedStringWithAggregatesFilterNotMin :: Maybe NestedStringFilter
    <*> arbitraryReducedMaybe n -- nestedStringWithAggregatesFilterNotContains :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringWithAggregatesFilterNotEndsWith :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringWithAggregatesFilterNotEq :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringWithAggregatesFilterNotEquals :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringWithAggregatesFilterNotGt :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringWithAggregatesFilterNotGte :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringWithAggregatesFilterNotIn :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- nestedStringWithAggregatesFilterNotLt :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringWithAggregatesFilterNotLte :: Maybe Text
    <*> arbitraryReducedMaybe n -- nestedStringWithAggregatesFilterNotNot :: Maybe NestedStringWithAggregatesFilterNot
    <*> arbitraryReducedMaybe n -- nestedStringWithAggregatesFilterNotNotIn :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- nestedStringWithAggregatesFilterNotStartsWith :: Maybe Text
  
instance Arbitrary NullableStringFieldUpdateOperationsInput where
  arbitrary = sized genNullableStringFieldUpdateOperationsInput

genNullableStringFieldUpdateOperationsInput :: Int -> Gen NullableStringFieldUpdateOperationsInput
genNullableStringFieldUpdateOperationsInput n =
  NullableStringFieldUpdateOperationsInput
    <$> arbitraryReducedMaybe n -- nullableStringFieldUpdateOperationsInputSet :: Maybe Text
    <*> arbitraryReducedMaybe n -- nullableStringFieldUpdateOperationsInputUnset :: Maybe Bool
  
instance Arbitrary StringFieldUpdateOperationsInput where
  arbitrary = sized genStringFieldUpdateOperationsInput

genStringFieldUpdateOperationsInput :: Int -> Gen StringFieldUpdateOperationsInput
genStringFieldUpdateOperationsInput n =
  StringFieldUpdateOperationsInput
    <$> arbitraryReducedMaybe n -- stringFieldUpdateOperationsInputSet :: Maybe Text
  
instance Arbitrary StringFilter where
  arbitrary = sized genStringFilter

genStringFilter :: Int -> Gen StringFilter
genStringFilter n =
  StringFilter
    <$> arbitraryReducedMaybe n -- stringFilterContains :: Maybe Text
    <*> arbitraryReducedMaybe n -- stringFilterEndsWith :: Maybe Text
    <*> arbitraryReducedMaybe n -- stringFilterEq :: Maybe Text
    <*> arbitraryReducedMaybe n -- stringFilterEquals :: Maybe Text
    <*> arbitraryReducedMaybe n -- stringFilterGt :: Maybe Text
    <*> arbitraryReducedMaybe n -- stringFilterGte :: Maybe Text
    <*> arbitraryReducedMaybe n -- stringFilterIn :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- stringFilterLt :: Maybe Text
    <*> arbitraryReducedMaybe n -- stringFilterLte :: Maybe Text
    <*> arbitraryReducedMaybe n -- stringFilterMode :: Maybe QueryMode
    <*> arbitraryReducedMaybe n -- stringFilterNot :: Maybe NestedStringFilterNot
    <*> arbitraryReducedMaybe n -- stringFilterNotIn :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- stringFilterStartsWith :: Maybe Text
  
instance Arbitrary StringNullableFilter where
  arbitrary = sized genStringNullableFilter

genStringNullableFilter :: Int -> Gen StringNullableFilter
genStringNullableFilter n =
  StringNullableFilter
    <$> arbitraryReducedMaybe n -- stringNullableFilterContains :: Maybe Text
    <*> arbitraryReducedMaybe n -- stringNullableFilterEndsWith :: Maybe Text
    <*> arbitraryReducedMaybe n -- stringNullableFilterEq :: Maybe Text
    <*> arbitraryReducedMaybe n -- stringNullableFilterEquals :: Maybe Text
    <*> arbitraryReducedMaybe n -- stringNullableFilterGt :: Maybe Text
    <*> arbitraryReducedMaybe n -- stringNullableFilterGte :: Maybe Text
    <*> arbitraryReducedMaybe n -- stringNullableFilterIn :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- stringNullableFilterIsSet :: Maybe Bool
    <*> arbitraryReducedMaybe n -- stringNullableFilterLt :: Maybe Text
    <*> arbitraryReducedMaybe n -- stringNullableFilterLte :: Maybe Text
    <*> arbitraryReducedMaybe n -- stringNullableFilterMode :: Maybe QueryMode
    <*> arbitraryReducedMaybe n -- stringNullableFilterNot :: Maybe NestedStringNullableFilterNot
    <*> arbitraryReducedMaybe n -- stringNullableFilterNotIn :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- stringNullableFilterStartsWith :: Maybe Text
  
instance Arbitrary StringNullableWithAggregatesFilter where
  arbitrary = sized genStringNullableWithAggregatesFilter

genStringNullableWithAggregatesFilter :: Int -> Gen StringNullableWithAggregatesFilter
genStringNullableWithAggregatesFilter n =
  StringNullableWithAggregatesFilter
    <$> arbitraryReducedMaybe n -- stringNullableWithAggregatesFilterCount :: Maybe NestedIntNullableFilter
    <*> arbitraryReducedMaybe n -- stringNullableWithAggregatesFilterMax :: Maybe NestedStringNullableFilter
    <*> arbitraryReducedMaybe n -- stringNullableWithAggregatesFilterMin :: Maybe NestedStringNullableFilter
    <*> arbitraryReducedMaybe n -- stringNullableWithAggregatesFilterContains :: Maybe Text
    <*> arbitraryReducedMaybe n -- stringNullableWithAggregatesFilterEndsWith :: Maybe Text
    <*> arbitraryReducedMaybe n -- stringNullableWithAggregatesFilterEq :: Maybe Text
    <*> arbitraryReducedMaybe n -- stringNullableWithAggregatesFilterEquals :: Maybe Text
    <*> arbitraryReducedMaybe n -- stringNullableWithAggregatesFilterGt :: Maybe Text
    <*> arbitraryReducedMaybe n -- stringNullableWithAggregatesFilterGte :: Maybe Text
    <*> arbitraryReducedMaybe n -- stringNullableWithAggregatesFilterIn :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- stringNullableWithAggregatesFilterIsSet :: Maybe Bool
    <*> arbitraryReducedMaybe n -- stringNullableWithAggregatesFilterLt :: Maybe Text
    <*> arbitraryReducedMaybe n -- stringNullableWithAggregatesFilterLte :: Maybe Text
    <*> arbitraryReducedMaybe n -- stringNullableWithAggregatesFilterMode :: Maybe QueryMode
    <*> arbitraryReducedMaybe n -- stringNullableWithAggregatesFilterNot :: Maybe NestedStringNullableWithAggregatesFilterNot
    <*> arbitraryReducedMaybe n -- stringNullableWithAggregatesFilterNotIn :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- stringNullableWithAggregatesFilterStartsWith :: Maybe Text
  
instance Arbitrary StringWithAggregatesFilter where
  arbitrary = sized genStringWithAggregatesFilter

genStringWithAggregatesFilter :: Int -> Gen StringWithAggregatesFilter
genStringWithAggregatesFilter n =
  StringWithAggregatesFilter
    <$> arbitraryReducedMaybe n -- stringWithAggregatesFilterCount :: Maybe NestedIntFilter
    <*> arbitraryReducedMaybe n -- stringWithAggregatesFilterMax :: Maybe NestedStringFilter
    <*> arbitraryReducedMaybe n -- stringWithAggregatesFilterMin :: Maybe NestedStringFilter
    <*> arbitraryReducedMaybe n -- stringWithAggregatesFilterContains :: Maybe Text
    <*> arbitraryReducedMaybe n -- stringWithAggregatesFilterEndsWith :: Maybe Text
    <*> arbitraryReducedMaybe n -- stringWithAggregatesFilterEq :: Maybe Text
    <*> arbitraryReducedMaybe n -- stringWithAggregatesFilterEquals :: Maybe Text
    <*> arbitraryReducedMaybe n -- stringWithAggregatesFilterGt :: Maybe Text
    <*> arbitraryReducedMaybe n -- stringWithAggregatesFilterGte :: Maybe Text
    <*> arbitraryReducedMaybe n -- stringWithAggregatesFilterIn :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- stringWithAggregatesFilterLt :: Maybe Text
    <*> arbitraryReducedMaybe n -- stringWithAggregatesFilterLte :: Maybe Text
    <*> arbitraryReducedMaybe n -- stringWithAggregatesFilterMode :: Maybe QueryMode
    <*> arbitraryReducedMaybe n -- stringWithAggregatesFilterNot :: Maybe NestedStringWithAggregatesFilterNot
    <*> arbitraryReducedMaybe n -- stringWithAggregatesFilterNotIn :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- stringWithAggregatesFilterStartsWith :: Maybe Text
  
instance Arbitrary User where
  arbitrary = sized genUser

genUser :: Int -> Gen User
genUser n =
  User
    <$> arbitraryReducedMaybe n -- userEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- userId :: Maybe Text
    <*> arbitraryReducedMaybe n -- userName :: Maybe Text
    <*> arbitraryReducedMaybe n -- userPasswordhash :: Maybe Text
  
instance Arbitrary UserAggregateInput where
  arbitrary = sized genUserAggregateInput

genUserAggregateInput :: Int -> Gen UserAggregateInput
genUserAggregateInput n =
  UserAggregateInput
    <$> arbitraryReducedMaybe n -- userAggregateInputCount :: Maybe [E'Count]
    <*> arbitraryReducedMaybe n -- userAggregateInputMax :: Maybe [E'Max]
    <*> arbitraryReducedMaybe n -- userAggregateInputMin :: Maybe [E'Max]
  
instance Arbitrary UserCountAggregateOutputType where
  arbitrary = sized genUserCountAggregateOutputType

genUserCountAggregateOutputType :: Int -> Gen UserCountAggregateOutputType
genUserCountAggregateOutputType n =
  UserCountAggregateOutputType
    <$> arbitraryReducedMaybe n -- userCountAggregateOutputTypeAll :: Maybe Int
    <*> arbitraryReducedMaybe n -- userCountAggregateOutputTypeEmail :: Maybe Int
    <*> arbitraryReducedMaybe n -- userCountAggregateOutputTypeId :: Maybe Int
    <*> arbitraryReducedMaybe n -- userCountAggregateOutputTypeName :: Maybe Int
    <*> arbitraryReducedMaybe n -- userCountAggregateOutputTypePasswordhash :: Maybe Int
  
instance Arbitrary UserCountOrderByAggregateInput where
  arbitrary = sized genUserCountOrderByAggregateInput

genUserCountOrderByAggregateInput :: Int -> Gen UserCountOrderByAggregateInput
genUserCountOrderByAggregateInput n =
  UserCountOrderByAggregateInput
    <$> arbitraryReducedMaybe n -- userCountOrderByAggregateInputEmail :: Maybe SortOrder
    <*> arbitraryReducedMaybe n -- userCountOrderByAggregateInputId :: Maybe SortOrder
    <*> arbitraryReducedMaybe n -- userCountOrderByAggregateInputName :: Maybe SortOrder
    <*> arbitraryReducedMaybe n -- userCountOrderByAggregateInputPasswordhash :: Maybe SortOrder
  
instance Arbitrary UserCreateInput where
  arbitrary = sized genUserCreateInput

genUserCreateInput :: Int -> Gen UserCreateInput
genUserCreateInput n =
  UserCreateInput
    <$> arbitrary -- userCreateInputEmail :: Text
    <*> arbitraryReducedMaybe n -- userCreateInputId :: Maybe Text
    <*> arbitraryReducedMaybe n -- userCreateInputName :: Maybe Text
    <*> arbitrary -- userCreateInputPasswordhash :: Text
  
instance Arbitrary UserCreateManyInput where
  arbitrary = sized genUserCreateManyInput

genUserCreateManyInput :: Int -> Gen UserCreateManyInput
genUserCreateManyInput n =
  UserCreateManyInput
    <$> arbitrary -- userCreateManyInputEmail :: Text
    <*> arbitraryReducedMaybe n -- userCreateManyInputId :: Maybe Text
    <*> arbitraryReducedMaybe n -- userCreateManyInputName :: Maybe Text
    <*> arbitrary -- userCreateManyInputPasswordhash :: Text
  
instance Arbitrary UserGroupByInput where
  arbitrary = sized genUserGroupByInput

genUserGroupByInput :: Int -> Gen UserGroupByInput
genUserGroupByInput n =
  UserGroupByInput
    <$> arbitraryReducedMaybe n -- userGroupByInputCount :: Maybe [E'Count]
    <*> arbitraryReducedMaybe n -- userGroupByInputMax :: Maybe [E'Max]
    <*> arbitraryReducedMaybe n -- userGroupByInputMin :: Maybe [E'Max]
    <*> arbitraryReducedMaybe n -- userGroupByInputEmail :: Maybe Bool
    <*> arbitraryReducedMaybe n -- userGroupByInputId :: Maybe Bool
    <*> arbitraryReducedMaybe n -- userGroupByInputName :: Maybe Bool
    <*> arbitraryReducedMaybe n -- userGroupByInputPasswordhash :: Maybe Bool
  
instance Arbitrary UserGroupByOutputType where
  arbitrary = sized genUserGroupByOutputType

genUserGroupByOutputType :: Int -> Gen UserGroupByOutputType
genUserGroupByOutputType n =
  UserGroupByOutputType
    <$> arbitraryReducedMaybe n -- userGroupByOutputTypeCount :: Maybe UserCountAggregateOutputType
    <*> arbitraryReducedMaybe n -- userGroupByOutputTypeMax :: Maybe UserMaxAggregateOutputType
    <*> arbitraryReducedMaybe n -- userGroupByOutputTypeMin :: Maybe UserMinAggregateOutputType
    <*> arbitraryReducedMaybe n -- userGroupByOutputTypeEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- userGroupByOutputTypeId :: Maybe Text
    <*> arbitraryReducedMaybe n -- userGroupByOutputTypeName :: Maybe Text
    <*> arbitraryReducedMaybe n -- userGroupByOutputTypePasswordhash :: Maybe Text
  
instance Arbitrary UserMaxAggregateOutputType where
  arbitrary = sized genUserMaxAggregateOutputType

genUserMaxAggregateOutputType :: Int -> Gen UserMaxAggregateOutputType
genUserMaxAggregateOutputType n =
  UserMaxAggregateOutputType
    <$> arbitraryReducedMaybe n -- userMaxAggregateOutputTypeEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- userMaxAggregateOutputTypeId :: Maybe Text
    <*> arbitraryReducedMaybe n -- userMaxAggregateOutputTypeName :: Maybe Text
    <*> arbitraryReducedMaybe n -- userMaxAggregateOutputTypePasswordhash :: Maybe Text
  
instance Arbitrary UserMaxOrderByAggregateInput where
  arbitrary = sized genUserMaxOrderByAggregateInput

genUserMaxOrderByAggregateInput :: Int -> Gen UserMaxOrderByAggregateInput
genUserMaxOrderByAggregateInput n =
  UserMaxOrderByAggregateInput
    <$> arbitraryReducedMaybe n -- userMaxOrderByAggregateInputEmail :: Maybe SortOrder
    <*> arbitraryReducedMaybe n -- userMaxOrderByAggregateInputId :: Maybe SortOrder
    <*> arbitraryReducedMaybe n -- userMaxOrderByAggregateInputName :: Maybe SortOrder
    <*> arbitraryReducedMaybe n -- userMaxOrderByAggregateInputPasswordhash :: Maybe SortOrder
  
instance Arbitrary UserMinAggregateOutputType where
  arbitrary = sized genUserMinAggregateOutputType

genUserMinAggregateOutputType :: Int -> Gen UserMinAggregateOutputType
genUserMinAggregateOutputType n =
  UserMinAggregateOutputType
    <$> arbitraryReducedMaybe n -- userMinAggregateOutputTypeEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- userMinAggregateOutputTypeId :: Maybe Text
    <*> arbitraryReducedMaybe n -- userMinAggregateOutputTypeName :: Maybe Text
    <*> arbitraryReducedMaybe n -- userMinAggregateOutputTypePasswordhash :: Maybe Text
  
instance Arbitrary UserMinOrderByAggregateInput where
  arbitrary = sized genUserMinOrderByAggregateInput

genUserMinOrderByAggregateInput :: Int -> Gen UserMinOrderByAggregateInput
genUserMinOrderByAggregateInput n =
  UserMinOrderByAggregateInput
    <$> arbitraryReducedMaybe n -- userMinOrderByAggregateInputEmail :: Maybe SortOrder
    <*> arbitraryReducedMaybe n -- userMinOrderByAggregateInputId :: Maybe SortOrder
    <*> arbitraryReducedMaybe n -- userMinOrderByAggregateInputName :: Maybe SortOrder
    <*> arbitraryReducedMaybe n -- userMinOrderByAggregateInputPasswordhash :: Maybe SortOrder
  
instance Arbitrary UserOrderByWithAggregationInput where
  arbitrary = sized genUserOrderByWithAggregationInput

genUserOrderByWithAggregationInput :: Int -> Gen UserOrderByWithAggregationInput
genUserOrderByWithAggregationInput n =
  UserOrderByWithAggregationInput
    <$> arbitraryReducedMaybe n -- userOrderByWithAggregationInputCount :: Maybe UserCountOrderByAggregateInput
    <*> arbitraryReducedMaybe n -- userOrderByWithAggregationInputMax :: Maybe UserMaxOrderByAggregateInput
    <*> arbitraryReducedMaybe n -- userOrderByWithAggregationInputMin :: Maybe UserMinOrderByAggregateInput
    <*> arbitraryReducedMaybe n -- userOrderByWithAggregationInputEmail :: Maybe SortOrder
    <*> arbitraryReducedMaybe n -- userOrderByWithAggregationInputId :: Maybe SortOrder
    <*> arbitraryReducedMaybe n -- userOrderByWithAggregationInputName :: Maybe SortOrder
    <*> arbitraryReducedMaybe n -- userOrderByWithAggregationInputPasswordhash :: Maybe SortOrder
  
instance Arbitrary UserOrderByWithRelationInput where
  arbitrary = sized genUserOrderByWithRelationInput

genUserOrderByWithRelationInput :: Int -> Gen UserOrderByWithRelationInput
genUserOrderByWithRelationInput n =
  UserOrderByWithRelationInput
    <$> arbitraryReducedMaybe n -- userOrderByWithRelationInputEmail :: Maybe SortOrder
    <*> arbitraryReducedMaybe n -- userOrderByWithRelationInputId :: Maybe SortOrder
    <*> arbitraryReducedMaybe n -- userOrderByWithRelationInputName :: Maybe SortOrder
    <*> arbitraryReducedMaybe n -- userOrderByWithRelationInputPasswordhash :: Maybe SortOrder
  
instance Arbitrary UserScalarWhereWithAggregatesInput where
  arbitrary = sized genUserScalarWhereWithAggregatesInput

genUserScalarWhereWithAggregatesInput :: Int -> Gen UserScalarWhereWithAggregatesInput
genUserScalarWhereWithAggregatesInput n =
  UserScalarWhereWithAggregatesInput
    <$> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputAnd :: Maybe UserScalarWhereWithAggregatesInputAND
    <*> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputNot :: Maybe UserScalarWhereWithAggregatesInputAND
    <*> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputOr :: Maybe [UserScalarWhereWithAggregatesInput]
    <*> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputEmail :: Maybe UserScalarWhereWithAggregatesInputEmail
    <*> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputId :: Maybe UserScalarWhereWithAggregatesInputEmail
    <*> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputName :: Maybe UserScalarWhereWithAggregatesInputName
    <*> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputPasswordhash :: Maybe UserScalarWhereWithAggregatesInputEmail
  
instance Arbitrary UserScalarWhereWithAggregatesInputAND where
  arbitrary = sized genUserScalarWhereWithAggregatesInputAND

genUserScalarWhereWithAggregatesInputAND :: Int -> Gen UserScalarWhereWithAggregatesInputAND
genUserScalarWhereWithAggregatesInputAND n =
  UserScalarWhereWithAggregatesInputAND
    <$> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputANDAnd :: Maybe UserScalarWhereWithAggregatesInputAND
    <*> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputANDNot :: Maybe UserScalarWhereWithAggregatesInputAND
    <*> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputANDOr :: Maybe [UserScalarWhereWithAggregatesInput]
    <*> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputANDEmail :: Maybe UserScalarWhereWithAggregatesInputEmail
    <*> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputANDId :: Maybe UserScalarWhereWithAggregatesInputEmail
    <*> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputANDName :: Maybe UserScalarWhereWithAggregatesInputName
    <*> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputANDPasswordhash :: Maybe UserScalarWhereWithAggregatesInputEmail
  
instance Arbitrary UserScalarWhereWithAggregatesInputEmail where
  arbitrary = sized genUserScalarWhereWithAggregatesInputEmail

genUserScalarWhereWithAggregatesInputEmail :: Int -> Gen UserScalarWhereWithAggregatesInputEmail
genUserScalarWhereWithAggregatesInputEmail n =
  UserScalarWhereWithAggregatesInputEmail
    <$> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputEmailCount :: Maybe NestedIntFilter
    <*> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputEmailMax :: Maybe NestedStringFilter
    <*> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputEmailMin :: Maybe NestedStringFilter
    <*> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputEmailContains :: Maybe Text
    <*> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputEmailEndsWith :: Maybe Text
    <*> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputEmailEq :: Maybe Text
    <*> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputEmailEquals :: Maybe Text
    <*> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputEmailGt :: Maybe Text
    <*> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputEmailGte :: Maybe Text
    <*> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputEmailIn :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputEmailLt :: Maybe Text
    <*> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputEmailLte :: Maybe Text
    <*> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputEmailMode :: Maybe QueryMode
    <*> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputEmailNot :: Maybe NestedStringWithAggregatesFilterNot
    <*> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputEmailNotIn :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputEmailStartsWith :: Maybe Text
  
instance Arbitrary UserScalarWhereWithAggregatesInputName where
  arbitrary = sized genUserScalarWhereWithAggregatesInputName

genUserScalarWhereWithAggregatesInputName :: Int -> Gen UserScalarWhereWithAggregatesInputName
genUserScalarWhereWithAggregatesInputName n =
  UserScalarWhereWithAggregatesInputName
    <$> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputNameCount :: Maybe NestedIntNullableFilter
    <*> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputNameMax :: Maybe NestedStringNullableFilter
    <*> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputNameMin :: Maybe NestedStringNullableFilter
    <*> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputNameContains :: Maybe Text
    <*> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputNameEndsWith :: Maybe Text
    <*> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputNameEq :: Maybe Text
    <*> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputNameEquals :: Maybe Text
    <*> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputNameGt :: Maybe Text
    <*> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputNameGte :: Maybe Text
    <*> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputNameIn :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputNameIsSet :: Maybe Bool
    <*> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputNameLt :: Maybe Text
    <*> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputNameLte :: Maybe Text
    <*> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputNameMode :: Maybe QueryMode
    <*> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputNameNot :: Maybe NestedStringNullableWithAggregatesFilterNot
    <*> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputNameNotIn :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- userScalarWhereWithAggregatesInputNameStartsWith :: Maybe Text
  
instance Arbitrary UserSelectInput where
  arbitrary = sized genUserSelectInput

genUserSelectInput :: Int -> Gen UserSelectInput
genUserSelectInput n =
  UserSelectInput
    <$> arbitraryReducedMaybe n -- userSelectInputRelated :: Maybe Bool
    <*> arbitraryReducedMaybe n -- userSelectInputScalars :: Maybe Bool
    <*> arbitraryReducedMaybe n -- userSelectInputEmail :: Maybe Bool
    <*> arbitraryReducedMaybe n -- userSelectInputId :: Maybe Bool
    <*> arbitraryReducedMaybe n -- userSelectInputName :: Maybe Bool
    <*> arbitraryReducedMaybe n -- userSelectInputPasswordhash :: Maybe Bool
  
instance Arbitrary UserUpdateInput where
  arbitrary = sized genUserUpdateInput

genUserUpdateInput :: Int -> Gen UserUpdateInput
genUserUpdateInput n =
  UserUpdateInput
    <$> arbitraryReducedMaybe n -- userUpdateInputEmail :: Maybe UserUpdateInputEmail
    <*> arbitraryReducedMaybe n -- userUpdateInputName :: Maybe UserUpdateInputName
    <*> arbitraryReducedMaybe n -- userUpdateInputPasswordhash :: Maybe UserUpdateInputEmail
  
instance Arbitrary UserUpdateInputEmail where
  arbitrary = sized genUserUpdateInputEmail

genUserUpdateInputEmail :: Int -> Gen UserUpdateInputEmail
genUserUpdateInputEmail n =
  UserUpdateInputEmail
    <$> arbitraryReducedMaybe n -- userUpdateInputEmailSet :: Maybe Text
  
instance Arbitrary UserUpdateInputName where
  arbitrary = sized genUserUpdateInputName

genUserUpdateInputName :: Int -> Gen UserUpdateInputName
genUserUpdateInputName n =
  UserUpdateInputName
    <$> arbitraryReducedMaybe n -- userUpdateInputNameSet :: Maybe Text
    <*> arbitraryReducedMaybe n -- userUpdateInputNameUnset :: Maybe Bool
  
instance Arbitrary UserUpdateManyInput where
  arbitrary = sized genUserUpdateManyInput

genUserUpdateManyInput :: Int -> Gen UserUpdateManyInput
genUserUpdateManyInput n =
  UserUpdateManyInput
    <$> arbitraryReducedMaybe n -- userUpdateManyInputEmail :: Maybe UserUpdateInputEmail
    <*> arbitraryReducedMaybe n -- userUpdateManyInputName :: Maybe UserUpdateInputName
    <*> arbitraryReducedMaybe n -- userUpdateManyInputPasswordhash :: Maybe UserUpdateInputEmail
  
instance Arbitrary UserWhereInput where
  arbitrary = sized genUserWhereInput

genUserWhereInput :: Int -> Gen UserWhereInput
genUserWhereInput n =
  UserWhereInput
    <$> arbitraryReducedMaybe n -- userWhereInputAnd :: Maybe UserWhereInputAND
    <*> arbitraryReducedMaybe n -- userWhereInputNot :: Maybe UserWhereInputAND
    <*> arbitraryReducedMaybe n -- userWhereInputOr :: Maybe [UserWhereInput]
    <*> arbitraryReducedMaybe n -- userWhereInputEmail :: Maybe UserWhereInputEmail
    <*> arbitraryReducedMaybe n -- userWhereInputId :: Maybe UserWhereInputEmail
    <*> arbitraryReducedMaybe n -- userWhereInputName :: Maybe UserWhereInputName
    <*> arbitraryReducedMaybe n -- userWhereInputPasswordhash :: Maybe UserWhereInputEmail
  
instance Arbitrary UserWhereInputAND where
  arbitrary = sized genUserWhereInputAND

genUserWhereInputAND :: Int -> Gen UserWhereInputAND
genUserWhereInputAND n =
  UserWhereInputAND
    <$> arbitraryReducedMaybe n -- userWhereInputANDAnd :: Maybe UserWhereInputAND
    <*> arbitraryReducedMaybe n -- userWhereInputANDNot :: Maybe UserWhereInputAND
    <*> arbitraryReducedMaybe n -- userWhereInputANDOr :: Maybe [UserWhereInput]
    <*> arbitraryReducedMaybe n -- userWhereInputANDEmail :: Maybe UserWhereInputEmail
    <*> arbitraryReducedMaybe n -- userWhereInputANDId :: Maybe UserWhereInputEmail
    <*> arbitraryReducedMaybe n -- userWhereInputANDName :: Maybe UserWhereInputName
    <*> arbitraryReducedMaybe n -- userWhereInputANDPasswordhash :: Maybe UserWhereInputEmail
  
instance Arbitrary UserWhereInputEmail where
  arbitrary = sized genUserWhereInputEmail

genUserWhereInputEmail :: Int -> Gen UserWhereInputEmail
genUserWhereInputEmail n =
  UserWhereInputEmail
    <$> arbitraryReducedMaybe n -- userWhereInputEmailContains :: Maybe Text
    <*> arbitraryReducedMaybe n -- userWhereInputEmailEndsWith :: Maybe Text
    <*> arbitraryReducedMaybe n -- userWhereInputEmailEq :: Maybe Text
    <*> arbitraryReducedMaybe n -- userWhereInputEmailEquals :: Maybe Text
    <*> arbitraryReducedMaybe n -- userWhereInputEmailGt :: Maybe Text
    <*> arbitraryReducedMaybe n -- userWhereInputEmailGte :: Maybe Text
    <*> arbitraryReducedMaybe n -- userWhereInputEmailIn :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- userWhereInputEmailLt :: Maybe Text
    <*> arbitraryReducedMaybe n -- userWhereInputEmailLte :: Maybe Text
    <*> arbitraryReducedMaybe n -- userWhereInputEmailMode :: Maybe QueryMode
    <*> arbitraryReducedMaybe n -- userWhereInputEmailNot :: Maybe NestedStringFilterNot
    <*> arbitraryReducedMaybe n -- userWhereInputEmailNotIn :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- userWhereInputEmailStartsWith :: Maybe Text
  
instance Arbitrary UserWhereInputName where
  arbitrary = sized genUserWhereInputName

genUserWhereInputName :: Int -> Gen UserWhereInputName
genUserWhereInputName n =
  UserWhereInputName
    <$> arbitraryReducedMaybe n -- userWhereInputNameContains :: Maybe Text
    <*> arbitraryReducedMaybe n -- userWhereInputNameEndsWith :: Maybe Text
    <*> arbitraryReducedMaybe n -- userWhereInputNameEq :: Maybe Text
    <*> arbitraryReducedMaybe n -- userWhereInputNameEquals :: Maybe Text
    <*> arbitraryReducedMaybe n -- userWhereInputNameGt :: Maybe Text
    <*> arbitraryReducedMaybe n -- userWhereInputNameGte :: Maybe Text
    <*> arbitraryReducedMaybe n -- userWhereInputNameIn :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- userWhereInputNameIsSet :: Maybe Bool
    <*> arbitraryReducedMaybe n -- userWhereInputNameLt :: Maybe Text
    <*> arbitraryReducedMaybe n -- userWhereInputNameLte :: Maybe Text
    <*> arbitraryReducedMaybe n -- userWhereInputNameMode :: Maybe QueryMode
    <*> arbitraryReducedMaybe n -- userWhereInputNameNot :: Maybe NestedStringNullableFilterNot
    <*> arbitraryReducedMaybe n -- userWhereInputNameNotIn :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- userWhereInputNameStartsWith :: Maybe Text
  
instance Arbitrary UserWhereUniqueInput where
  arbitrary = sized genUserWhereUniqueInput

genUserWhereUniqueInput :: Int -> Gen UserWhereUniqueInput
genUserWhereUniqueInput n =
  UserWhereUniqueInput
    <$> arbitraryReducedMaybe n -- userWhereUniqueInputEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- userWhereUniqueInputId :: Maybe Text
  



instance Arbitrary E'Count where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Max where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary QueryMode where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary SortOrder where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary UserScalarFieldEnum where
  arbitrary = arbitraryBoundedEnum

