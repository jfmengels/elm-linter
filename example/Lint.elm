module Lint exposing (lint, LintRule, Errors, Node)

import Ast.Expression exposing (..)
import Ast.Statement exposing (..)


type Node
    = Statement
    | Expression
    | Type


type alias Errors =
    List String


type alias LintStatementImplementation context =
    context -> Statement -> ( Errors, context )


type alias LintTypeImplementation context =
    context -> Type -> ( Errors, context )


type alias LintExpressionImplementation context =
    context -> Expression -> ( Errors, context )


type alias LintRule context =
    { statementFn : LintStatementImplementation context
    , typeFn : LintTypeImplementation context
    , expressionFn : LintExpressionImplementation context
    , context : context
    }


visitStatement : LintRule context -> context -> Statement -> ( Errors, context )
visitStatement rule context node =
    let
        ( errorsParent, ctxParent ) =
            rule.statementFn context node

        ( errorsChildren, ctxChildren ) =
            case node of
                FunctionTypeDeclaration name application ->
                    rule.typeFn ctxParent application

                FunctionDeclaration name params body ->
                    rule.expressionFn ctxParent body

                _ ->
                    ( [], context )
    in
        ( List.concat [ errorsParent, errorsChildren ], ctxChildren )


lint : List Statement -> LintRule context -> Errors
lint statements rule =
    let
        ( allErrors, _ ) =
            List.foldl
                (\node ( errors, ctx ) ->
                    let
                        ( errors_, ctx_ ) =
                            visitStatement rule ctx node
                    in
                        ( errors ++ errors_, ctx_ )
                )
                ( [], rule.context )
                statements
    in
        allErrors
