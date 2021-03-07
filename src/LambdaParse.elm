module LambdaParse exposing (..)

import Parser exposing (..)


type Expr
    = Variable Char
    | Head Char Expr
    | Group Expr
    | Chain ( Expr, Expr )


parseExpr : Parser Expr
parseExpr =
    succeed identity
        |= oneOf
            [ backtrackable (lazy (\_ -> parseChain) |> log "parseChain")
            , lazy (\_ -> parseHead) |> log "parseHead"
            , lazy (\_ -> parseGroup) |> log "parseGroup"
            , parseVariable |> log "parseVariable"
            ]


parseChain : Parser Expr
parseChain =
    let
        parsePair =
            succeed Tuple.pair
                |= parseChainStart
                |. spaces
                |= parseExpr
    in
    map Chain parsePair


parseChainStart : Parser Expr
parseChainStart =
    succeed identity
        |= oneOf
            [ lazy (\_ -> parseHead) |> log "chainStart -> parseHead"
            , lazy (\_ -> parseGroup) |> log "chainStart -> parseGroup"
            , parseVariable |> log "chainStart -> parseVariable"
            ]


parseVariable : Parser Expr
parseVariable =
    succeed Variable
        |= parseChar


parseHead : Parser Expr
parseHead =
    succeed Head
        |. symbol "λ"
        |= parseChar
        |. symbol "."
        |= parseExpr


parseGroup : Parser Expr
parseGroup =
    succeed Group
        |. symbol "("
        |= parseExpr
        |. symbol ")"


parseChar : Parser Char
parseChar =
    let
        charFromMaybe input =
            case input of
                Just ( value, _ ) ->
                    succeed value

                Nothing ->
                    problem "Needs to be a lower-case character"
    in
    succeed ()
        |. chompIf Char.isLower
        |> getChompedString
        |> map (\s -> String.uncons s)
        |> andThen charFromMaybe


log : String -> Parser a -> Parser a
log message parser =
    succeed ()
        |> andThen
            (\() ->
                let
                    _ =
                        Debug.log "starting" message
                in
                succeed
                    (\source offsetBefore parseResult offsetAfter ->
                        let
                            _ =
                                Debug.log "-----------------------------------------------" message

                            _ =
                                Debug.log "source         " source

                            _ =
                                Debug.log "chomped string " (String.slice offsetBefore offsetAfter source)

                            _ =
                                Debug.log "parsed result  " parseResult
                        in
                        parseResult
                    )
                    |= getSource
                    |= getOffset
                    |= parser
                    |= getOffset
            )
