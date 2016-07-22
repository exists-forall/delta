{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module ExternalProblem where

import Data.Text (Text)
import GHC.Generics

import Data.Map (Map)

import Data.Aeson

{- Stylistic note:
  Even though it is not stylistically idiomatic Haskell, underscores are used in this module for
  field names because it is more consistent with the style generally used for JSON objects.
-}

newtype AtomIdent = AtomIdent [Text]
  deriving (Generic, Eq, Ord, Show, ToJSON, FromJSON)

newtype InteractionIdent = InteractionIdent [Text]
  deriving (Generic, Eq, Ord, Show, ToJSON, FromJSON)

data Conversion = Conversion
  { subtype :: AtomIdent
  , supertype :: AtomIdent
  }
  deriving (Generic, Eq, Ord, Show, ToJSON, FromJSON)

newtype TypeVar = TypeVar Text
  deriving (Generic, Eq, Ord, Show, ToJSON, FromJSON)

newtype BoundVar = BoundVar Text
  deriving (Generic, Eq, Ord, Show, ToJSON, FromJSON)

data PolyType
  = AtomType AtomIdent
  | QuantifiedVariable BoundVar
  | TypeApplication
    { head_type :: PolyType
    , param_type :: PolyType
    }
  | FunctionType
    { argument_type :: PolyType
    , return_type :: PolyType
    }
  | TupleType
    { first_type :: PolyType
    , second_type :: PolyType
    }
  | NeverType
  deriving (Generic, Eq, Ord, Show, ToJSON, FromJSON)

data Constraint
  = InstantiationConstraint
    { instantiation_constraint_var :: TypeVar
    , polymorphic_type :: PolyType
    }
  | SubtypeConstraint
    { subtype_var :: TypeVar
    , supertype_var :: TypeVar
    }
  | ExactEqualityConstraint
    { equality_var_1 :: TypeVar
    , equality_var_2 :: TypeVar
    }
  | TypeApplicationEqualityConstraint
    { applied_var :: TypeVar
    , head_var :: TypeVar
    , param_var :: TypeVar
    }
  | FunctionEqualityConstraint
    { function_var :: TypeVar
    , argument_var :: TypeVar
    , return_var :: TypeVar
    }
  | TupleEqualityConstraint
    { tuple_var :: TypeVar
    , first_var :: TypeVar
    , second_var :: TypeVar
    }
  deriving (Generic, Eq, Ord, Show, ToJSON, FromJSON)

type ProblemID = Text

data Problem
  = Problem
    { conversions :: [Conversion]
    , subproblems :: Map ProblemID [Constraint]
    }
  deriving (Generic, Eq, Ord, Show, ToJSON, FromJSON)

data TypeSolution
  = AtomTypeSolution AtomIdent
  | TypeApplicationSolution
    { head_solution :: TypeSolution
    , param_solution :: TypeSolution
    }
  | FunctionTypeSolution
    { argument_solution :: TypeSolution
    , return_sloution :: TypeSolution
    }
  | TupleTypeSolution
    { first_solution :: TypeSolution
    , second_solution :: TypeSolution
    }
  | NeverTypeSolution
  deriving (Generic, Eq, Ord, Show, ToJSON, FromJSON)

type UnwrappedTypeVar = Text

data SubProblemResult
  = Error
    { error_message :: Text
    , error_relevant_vars :: [TypeVar]
    }
  | Success
    { var_solutions :: Map UnwrappedTypeVar TypeSolution
    }
  deriving (Generic, Eq, Ord, Show, ToJSON, FromJSON)

data Results
  = FatalError
    { fatal_error_message :: Text
    }
  | Results
    { subproblem_results :: Map ProblemID SubProblemResult
    }
  deriving (Generic, Eq, Ord, Show, ToJSON, FromJSON)
