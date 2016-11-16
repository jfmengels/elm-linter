module Lint exposing (lint, findNoAnnotatedFunction)

import Ast.Expression exposing (..)
import Ast.Statement exposing (..)


type alias Error =
    String


type alias Errors =
    List Error


type alias Context =
    { annotatedFunctions : List String
    }


type alias Node =
    Statement


type alias LintRule =
    Context -> Node -> ( Errors, Context )


lint : List Statement -> LintRule -> Errors
lint statements rule =
    let
        ( allErrors, _ ) =
            List.foldl
                (\node ( errors, ctx ) ->
                    let
                        ( errors_, ctx_ ) =
                            rule ctx node

                        foo =
                            [ Debug.log "context" ctx
                            , Debug.log "context end" ctx_
                            ]
                    in
                        ( errors ++ errors_, ctx_ )
                )
                ( [], Context [] )
                statements
    in
        allErrors


findNoAnnotatedFunction : LintRule
findNoAnnotatedFunction ctx node =
    case node of
        FunctionTypeDeclaration name application ->
            ( [], { ctx | annotatedFunctions = (Debug.log "name" name) :: ctx.annotatedFunctions } )

        FunctionDeclaration name params body ->
            if List.member name ctx.annotatedFunctions then
                ( [], ctx )
            else
                ( [ name ++ " does not have a type declaration" ], ctx )

        _ ->
            ( [], ctx )
