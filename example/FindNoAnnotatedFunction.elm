module FindNoAnnotatedFunction exposing (rule)

import Lint exposing (LintRule, Errors, Node)
import Ast.Statement exposing (..)
import Ast.Expression exposing (..)


type alias Context =
    { annotatedFunctions : List String
    }


rule : LintRule Context
rule =
    { statementFn = statementFn
    , typeFn = typeFn
    , expressionFn = expressionFn
    , context = Context []
    }


expressionFn : Context -> Expression -> ( Errors, Context )
expressionFn ctx node =
    ( [], ctx )


typeFn : Context -> Type -> ( Errors, Context )
typeFn ctx node =
    ( [], ctx )


statementFn : Context -> Statement -> ( Errors, Context )
statementFn ctx node =
    case node of
        FunctionTypeDeclaration name application ->
            ( [], { ctx | annotatedFunctions = name :: ctx.annotatedFunctions } )

        FunctionDeclaration name params body ->
            if List.member name ctx.annotatedFunctions then
                ( [], ctx )
            else
                ( [ name ++ " does not have a type declaration" ], ctx )

        _ ->
            ( [], ctx )
