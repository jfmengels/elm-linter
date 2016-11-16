module FindNoAnnotatedFunction exposing (rule)

import Lint exposing (LintRule, Errors, Node)
import Ast.Expression exposing (..)
import Ast.Statement exposing (..)


type alias Context =
    { annotatedFunctions : List String
    }


rule : LintRule Context
rule =
    { implementation = implementation
    , context = Context []
    }


implementation : Context -> Node -> ( Errors, Context )
implementation ctx node =
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
