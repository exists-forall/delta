{-# LANGUAGE OverloadedStrings #-}

module HandleExternalProblemTests (test) where

import Data.Text (Text)

import Delta.Structures.TypeSolve
import HandleExternalProblem

import Test.Hspec

import qualified Data.Map as Map
import Data.Map (Map)

-- Trivial aliases for concision
a :: Text -> AtomIdent
a name = AtomIdent [name]

v :: Text -> TypeVar
v = TypeVar

q :: Text -> PolyType
q = QuantifiedVariable . BoundVar

pureType :: PolyType
pureType = InteractionType [] Nothing

pureSolution :: TypeSolution
pureSolution = InteractionTypeSolution []

-- More tests to come!
-- This is far from covering the entirety of the API, let alone the full range of its behaviors

wellFormedProblems :: Problem
wellFormedProblems = Problem
  { conversions =
    [ Conversion (a "T1") (a "T2")
    , Conversion (a "T2") (a "T3")
    , Conversion (a "T1") (a "T3")
    ]
  , subproblems = Map.fromList
    [ (,) "swap test"
      [ InstantiationConstraint (v "swap") $
        FunctionType
          (TupleType (q "a") (q "b"))
          pureType
          (TupleType (q "b") (q "a"))
      , SubtypeConstraint (v "swap") (v "func")
      , FunctionEqualityConstraint (v "func") (v "arg") (v "inter") (v "ret")
      , InstantiationConstraint (v "x") $ AtomType (a "Bool")
      , InstantiationConstraint (v "y") $ AtomType (a "String")
      , TupleEqualityConstraint (v "tuple") (v "x") (v "y")
      , SubtypeConstraint (v "tuple") (v "arg")
      ]

    , (,) "id test"
      [ InstantiationConstraint (v "id") $
        FunctionType (q "a") pureType (q "a")
      , InstantiationConstraint (v "x") $
        AtomType (a "T1")
      , SubtypeConstraint (v "x") (v "arg")
      , SubtypeConstraint (v "id") (v "func")
      , FunctionEqualityConstraint (v "func") (v "arg") (v "inter") (v "ret")
      , InstantiationConstraint (v "ret") $ AtomType (a "T2")
      ]

    , (,) "map test"
      [ InstantiationConstraint (v "map") $
        FunctionType
          (TupleType
            (FunctionType (q "a") pureType (q "b"))
            (TypeApplication (AtomType (a "Seq")) (q "a"))
          )
          pureType
          (TypeApplication (AtomType (a "Seq")) (q "b"))
      , SubtypeConstraint (v "map") (v "func")
      , FunctionEqualityConstraint (v "func") (v "arg") (v "inter") (v "ret")
      , SubtypeConstraint (v "params") (v "arg")
      , TupleEqualityConstraint (v "params") (v "lambda") (v "seq")
      , FunctionEqualityConstraint (v "lambda") (v "lamArg") (v "lamInter") (v "lamRet")
      , TupleEqualityConstraint (v "lamRet") (v "lamArg") (v "lamArg")

      , InstantiationConstraint (v "seq") $
        TypeApplication (AtomType (a "Maybe")) (AtomType (a "T1"))
      ]

    , (,) "contravariance test"
      [ InstantiationConstraint (v "f1") $
        (FunctionType (AtomType (a "T3")) pureType (AtomType (a "T1")))
      , InstantiationConstraint (v "f3") $
        (FunctionType (AtomType (a "T2")) pureType (AtomType (a "T3")))
      , SubtypeConstraint (v "f1") (v "f2")
      , SubtypeConstraint (v "f2") (v "f3")
      ]
    ]
  }

illFormedProblems :: Problem
illFormedProblems = Problem
  { conversions =
    [ Conversion (a "T1") (a "T2")
    , Conversion (a "T2") (a "T3")
    , Conversion (a "T1") (a "T3")
    ]
  , subproblems = Map.fromList
    [ (,) "infinite loop test"
      [ TupleEqualityConstraint (v "x") (v "y") (v "y")
      , TupleEqualityConstraint (v "y") (v "x") (v "x")
      ]
    , (,) "unsatisfiable inequality test"
      [ InstantiationConstraint (v "lo") $ AtomType (a "T3")
      , InstantiationConstraint (v "hi") $ AtomType (a "T1")
      , SubtypeConstraint (v "lo") (v "mid")
      , SubtypeConstraint (v "mid") (v "hi")
      ]
    ]
  }

{- These results were *not* generated by hand.
They are just the output of the solver, which have been deemed by a human to "look right."
A simple equality test is used to check that the results of the solver match these results.
This approach to testing is fragile and sub-optimal, because:

  1. Humans are not good at spotting bugs in the output, which will become especially important as
     the test problems inevitably grow more complex.
  2. The solver is supposed to generate a solution, but not necessarily a *unique* solution, so
     testing for exact equality against an expected result isn't quite correct.
  3. The error messages displayed if the equality test fails will be difficult to interpret, because
     they will not localize the failure to a particular constraint, particular type variable(s), or
     even a particular sub-problem.

In the future, it would be great to get ride of this `expectedResults` object and develop an
automated way to test the validity of a solution based on the constraints.  This shouldn't be too
hard; basically just iterate through every constraint and check that it holds.
-}
expectedResults :: Results
expectedResults = Results
  { subproblem_results =
      Map.fromList
        [ ( "contravariance test"
          , Success
              { var_solutions =
                  Map.fromList
                    [ ( "f1"
                      , FunctionTypeSolution
                          { argument_solution = AtomTypeSolution (AtomIdent [ "T3" ])
                          , interaction_solution = pureSolution
                          , return_sloution = AtomTypeSolution (AtomIdent [ "T1" ])
                          }
                      )
                    , ( "f2"
                      , FunctionTypeSolution
                          { argument_solution = AtomTypeSolution (AtomIdent [ "T2" ])
                          , interaction_solution = pureSolution
                          , return_sloution = AtomTypeSolution (AtomIdent [ "T1" ])
                          }
                      )
                    , ( "f3"
                      , FunctionTypeSolution
                          { argument_solution = AtomTypeSolution (AtomIdent [ "T2" ])
                          , interaction_solution = pureSolution
                          , return_sloution = AtomTypeSolution (AtomIdent [ "T3" ])
                          }
                      )
                    ]
              }
          )
        , ( "id test"
          , Success
              { var_solutions =
                  Map.fromList
                    [ ( "arg" , AtomTypeSolution (AtomIdent [ "T1" ]) )
                    , ( "func"
                      , FunctionTypeSolution
                          { argument_solution = AtomTypeSolution (AtomIdent [ "T1" ])
                          , interaction_solution = pureSolution
                          , return_sloution = AtomTypeSolution (AtomIdent [ "T2" ])
                          }
                      )
                    , ( "id"
                      , FunctionTypeSolution
                          { argument_solution = AtomTypeSolution (AtomIdent [ "T1" ])
                          , interaction_solution = pureSolution
                          , return_sloution = AtomTypeSolution (AtomIdent [ "T1" ])
                          }
                      )
                    , ( "ret" , AtomTypeSolution (AtomIdent [ "T2" ]) )
                    , ( "inter", pureSolution )
                    , ( "x" , AtomTypeSolution (AtomIdent [ "T1" ]) )
                    ]
              }
          )
        , ( "map test"
          , Error
              { error_message =
                  "I can't find any type that could work here.\nI'm inferring that this type must be convertible from:\n    Maybe\nBut also that it must be convertible to:\n    Seq"
              , error_relevant_vars = [ TypeVar "arg" ]
              }
          )
        , ( "swap test"
          , Success
              { var_solutions =
                  Map.fromList
                    [ ( "arg"
                      , TupleTypeSolution
                          { first_solution = AtomTypeSolution (AtomIdent [ "Bool" ])
                          , second_solution = AtomTypeSolution (AtomIdent [ "String" ])
                          }
                      )
                    , ( "func"
                      , FunctionTypeSolution
                          { argument_solution =
                              TupleTypeSolution
                                { first_solution = AtomTypeSolution (AtomIdent [ "Bool" ])
                                , second_solution = AtomTypeSolution (AtomIdent [ "String" ])
                                }
                          , interaction_solution = pureSolution
                          , return_sloution =
                              TupleTypeSolution
                                { first_solution = AtomTypeSolution (AtomIdent [ "String" ])
                                , second_solution = AtomTypeSolution (AtomIdent [ "Bool" ])
                                }
                          }
                      )
                    , ( "inter", pureSolution )
                    , ( "ret"
                      , TupleTypeSolution
                          { first_solution = AtomTypeSolution (AtomIdent [ "String" ])
                          , second_solution = AtomTypeSolution (AtomIdent [ "Bool" ])
                          }
                      )
                    , ( "swap"
                      , FunctionTypeSolution
                          { argument_solution =
                              TupleTypeSolution
                                { first_solution = AtomTypeSolution (AtomIdent [ "Bool" ])
                                , second_solution = AtomTypeSolution (AtomIdent [ "String" ])
                                }
                          , interaction_solution = pureSolution
                          , return_sloution =
                              TupleTypeSolution
                                { first_solution = AtomTypeSolution (AtomIdent [ "String" ])
                                , second_solution = AtomTypeSolution (AtomIdent [ "Bool" ])
                                }
                          }
                      )
                    , ( "tuple"
                      , TupleTypeSolution
                          { first_solution = AtomTypeSolution (AtomIdent [ "Bool" ])
                          , second_solution = AtomTypeSolution (AtomIdent [ "String" ])
                          }
                      )
                    , ( "x" , AtomTypeSolution (AtomIdent [ "Bool" ]) )
                    , ( "y" , AtomTypeSolution (AtomIdent [ "String" ]) )
                    ]
              }
          )
        ]
  }

isFailedSubResult :: SubProblemResult -> Bool
isFailedSubResult (Success _) = False
isFailedSubResult (Error _ _) = True

areResultsWhereAllFailed :: Results -> Bool
areResultsWhereAllFailed (Results subResults) = all isFailedSubResult subResults
areResultsWhereAllFailed (FatalError _) = False

test :: Spec
test = describe "HandleExternalProblem" $ do
  it "solves well-formed problems" $
    solveExternal wellFormedProblems `shouldBe` expectedResults

  it "generates errors for ill-formed sub-problems" $
    solveExternal illFormedProblems `shouldSatisfy` areResultsWhereAllFailed
