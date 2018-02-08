{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
module Main where

import Control.Exception

import Data.Ix
import Data.SearchEngine -- from full-text-search package
import Data.Time

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
  putStrLn "Spinning up."
  let demoRecords = [ (RecipeDescription 1 ["See"])
                    , (RecipeDescription 2 ["See", "Jane"])
                    , (RecipeDescription 3 ["See", "Jane", "Run"])
                    ]
  let searchEngine = insertDocs demoRecords initialRecipeSearchEngine

  putStrLn "Constructing demo index..."
  printTiming "Done!" $
    evaluate searchEngine >> return ()
  putStrLn $ "search engine invariant: " ++ show (invariant searchEngine)
  let rankedResultsSee  = query searchEngine $ [T.pack "See"]
      rankedResultsJane = query searchEngine $ [T.pack "Jane"]
      rankedResultsRun  = query searchEngine $ [T.pack "Run"]
      rankedResultsCase = query searchEngine $ [T.pack "SEE"]

  putStrLn "Results:"
  putStrLn (show rankedResultsSee)
  putStrLn (show rankedResultsJane)
  putStrLn (show rankedResultsRun)
  putStrLn (show rankedResultsCase)
  putStrLn "fin!"

printTiming :: String -> IO () -> IO ()
printTiming msg action = do
  t <- getCurrentTime
  action
  t' <- getCurrentTime
  putStrLn (msg ++ ". time: " ++ show (diffUTCTime t' t))

