module LambdaParse exposing (Term(..), parseTerm)

import Parser exposing (..)
import Set exposing (Set, fromList)


type Term
    = Variable Char
    | Abstraction (Set Char) Term
    | Group Term
    | Application Term Term


parseTerm : Parser Term
parseTerm =
    succeed identity
        |= oneOf
            [ backtrackable (lazy (\_ -> parseApplication))
            , lazy (\_ -> parseAbstraction)
            , lazy (\_ -> parseGroup)
            , parseVariable
            ]


parseApplication : Parser Term
parseApplication =
    succeed Application
        |= parseApplicationStart
        |. spaces
        |= parseTerm



{-
   If we allowed parseApplication to have any type of Term as its first value,
   it would be allowed to have an Application, and would end in an infinite
   loop of Application Terms. By forcing the first term to be something concrete
   we avoid this.
-}


parseApplicationStart : Parser Term
parseApplicationStart =
    succeed identity
        |= oneOf
            [ lazy (\_ -> parseAbstraction)
            , lazy (\_ -> parseGroup)
            , parseVariable
            ]


parseAbstraction : Parser Term
parseAbstraction =
    succeed Abstraction
        |. symbol "λ"
        |= parseCharSet
        |. symbol "."
        |= parseTerm


parseGroup : Parser Term
parseGroup =
    succeed Group
        |. symbol "("
        |= parseTerm
        |. symbol ")"


parseVariable : Parser Term
parseVariable =
    succeed Variable
        |= parseChar


parseCharSet : Parser (Set Char)
parseCharSet =
    let
        setFromList list =
            case list of
                [] ->
                    problem "Empty list"

                _ ->
                    succeed <| Set.fromList list
    in
    succeed ()
        |. chompWhile Char.isLower
        |> getChompedString
        |> map (\s -> String.toList s)
        |> andThen setFromList


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
