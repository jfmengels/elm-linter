module NoDebugLog exposing (rule)

import Lint exposing (LintRule, Errors, Node)
import Ast.Statement exposing (..)
import Ast.Expression exposing (..)


type alias Context =
    {}


rule : LintRule Context
rule =
    { statementFn = statementFn
    , typeFn = typeFn
    , expressionFn = expressionFn
    , context = Context
    }


expressionFn : Context -> Expression -> ( Errors, Context )
expressionFn ctx node =
    case node of
        Variable vars ->
            if List.member "Debug" (Debug.log "vars" vars) then
                ( [ "Forbidden use of Debug" ], ctx )
            else
                ( [], ctx )

        _ ->
            ( [], ctx )


typeFn : Context -> Type -> ( Errors, Context )
typeFn ctx node =
    ( [], ctx )


statementFn : Context -> Statement -> ( Errors, Context )
statementFn ctx node =
    ( [], ctx )
