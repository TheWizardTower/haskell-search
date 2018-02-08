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
  , transformQueryTerm   = normaliseQueryToken
  , documentFeatureValue = const noFeatures
  }

documentIndex :: RecipeDescription -> Index
documentIndex (RecipeDescription index _ ) = index

extractRecipeTokens :: RecipeDescription -> RecipeDocField -> Document
extractRecipeTokens (RecipeDescription index _) RecipeIndex = [(T.pack $ show index)]
extractRecipeTokens (RecipeDescription _ doc) RecipeDocument = doc

normaliseQueryToken :: Term -> RecipeDocField -> Term
normaliseQueryToken term RecipeIndex = term
normaliseQueryToken term RecipeDocument = T.toLower term

main :: IO ()
main = do
--  initSearchEngine -- needs args
  putStrLn "hello world"
