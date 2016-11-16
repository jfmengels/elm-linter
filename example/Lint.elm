module Lint exposing (lint, LintRule, LintRuleImplementation, Error, Errors, Node)

import Ast.Expression exposing (..)
import Ast.Statement exposing (..)


type alias Node =
    Statement


type alias Error =
    String


type alias Errors =
    List Error


type alias LintRuleImplementation context =
    context -> Node -> ( Errors, context )


type alias LintRule context =
    { implementation : LintRuleImplementation context
    , context : context
    }


lint : List Statement -> LintRule context -> Errors
lint statements { implementation, context } =
    let
        ( allErrors, _ ) =
            List.foldl
                (\node ( errors, ctx ) ->
                    let
                        ( errors_, ctx_ ) =
                            implementation ctx node
                    in
                        ( errors ++ errors_, ctx_ )
                )
                ( [], context )
                statements
    in
        allErrors
