module Census.Types where

import Data.ByteString


type Attribute = ByteString

type Label = Attribute
type Unlabeled = [Attribute]
data Labeled = Labeled Unlabeled Label deriving (Show, Eq)
data Result = TruePositive
            | TrueNegative
            | FalsePositive
            | FalseNegative
            deriving (Show, Eq)