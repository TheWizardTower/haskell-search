{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
module Main where

import Data.Ix
import Data.SearchEngine -- from full-text-search package

import qualified Data.Text as T

-- SearchConfig
--    documentKey :: doc -> key
--    extractDocumentTerms :: doc -> field -> [Term]
--    transformQueryTerm   :: Term -> field -> Term
--    documentFeatureValue :: doc -> feature -> Float

type RecipeSearchEngine = SearchEngine
      RecipeDescription
      Index
      RecipeDocField
      NoFeatures

data RecipeDocField = RecipeIndex
                    | RecipeDocument
                    deriving (Eq, Ord, Enum, Bounded, Ix, Show)

type Index = Int
type Document = [T.Text]

data RecipeDescription =
  RecipeDescription Index Document

recipeSearchConfig :: SearchConfig RecipeDescription Index RecipeDocField NoFeatures
recipeSearchConfig = SearchConfig
  { documentKey          = documentIndex
  , extractDocumentTerms = extractRecipeTokens
  , transformQueryTerm   = normalizeQueryToken
  , documentFeatureValue = const noFeatures
  }

documentIndex :: RecipeDescription -> Index
documentIndex (RecipeDescription index _ ) = index

extractRecipeTokens :: RecipeDescription -> RecipeDocField -> Document
extractRecipeTokens (RecipeDescription index _) RecipeIndex = [(T.pack $ show index)]
extractRecipeTokens (RecipeDescription _ doc) RecipeDocument = doc

normalizeQueryToken :: Term -> RecipeDocField -> Term
normalizeQueryToken term RecipeIndex = term
normalizeQueryToken term RecipeDocument = T.toLower term

initialRecipeSearchEngine :: RecipeSearchEngine
initialRecipeSearchEngine = initSearchEngine recipeSearchConfig defaultSearchRankParameters


defaultSearchRankParameters :: SearchRankParameters RecipeDocField NoFeatures
defaultSearchRankParameters =
  SearchRankParameters { paramK1
                       , paramB
                       , paramFieldWeights
                       , paramFeatureWeights = noFeatures
                       , paramFeatureFunctions = noFeatures
                       , paramResultsetSoftLimit = 200
                       , paramResultsetHardLimit = 400
                       , paramAutosuggestPrefilterLimit = 500
                       , paramAutosuggestPostfilterLimit = 500
                       }
  where
    paramK1 :: Float
    paramK1 = 1.5

    paramB :: RecipeDocField -> Float
    paramB RecipeIndex    = 1.0
    paramB RecipeDocument = 0.5

    paramFieldWeights :: RecipeDocField -> Float
    paramFieldWeights RecipeIndex    = 20
    paramFieldWeights RecipeDocument = 5

main :: IO ()
main = do
--  initSearchEngine -- needs args
  putStrLn "hello world"
