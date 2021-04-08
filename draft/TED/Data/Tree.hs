module TED.Data.Tree where

import           Control.Arrow (first, second)
import           Data.Vector   (Vector)
import qualified Data.Vector   as V

-- $setup
--
-- TODO: Useful Arbitrary instances!
--
-- >>> :set -XScopedTypeVariables
-- >>> import Test.QuickCheck
-- >>> instance Arbitrary a => Arbitrary (Forest a) where arbitrary = pure (Forest mempty)
-- >>> instance Arbitrary a => Arbitrary (Tree a) where arbitrary = Node <$> arbitrary <*> arbitrary

-- * Trees and Forests

-- | A 'Tree' is a node labelled with an element of 'sigma'.
data Tree sigma = Node
  { treeLabel    :: sigma
  , treeBranches :: Forest sigma
  }
  deriving (Ord, Eq)

-- | A 'Forest' is an ordered collection of 'Tree's.
newtype Forest sigma = Forest
  { forestTrees :: Vector (Tree sigma) }
  deriving (Ord, Eq)
