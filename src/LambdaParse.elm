module LambdaParse exposing (Term(..), parseTerm)

import Parser exposing (..)


type Term
    = Variable Char
    | Abstraction Char Term
    | Group Term
    | Application Term Term


parseTerm : Parser Term
parseTerm =
    loop [] termLoop
        |> andThen foldApplications


termLoop : List Term -> Parser (Step (List Term) (List Term))
termLoop reversedTerms =
    oneOf
        [ succeed (\term -> Loop (term :: reversedTerms))
            |= oneOf
                [ parseAbstraction
                , parseGroup
                , parseVariable
                ]
        , succeed ()
            |> map (\_ -> Done (List.reverse reversedTerms))
        ]


foldApplications : List Term -> Parser Term
foldApplications termsList =
    case termsList of
        [ t ] ->
            succeed t

        t :: rest ->
            succeed (List.foldl (\a b -> Application b a) t rest)

        [] ->
            problem "Somehow got an empty list in foldApplications"


parseAbstraction : Parser Term
parseAbstraction =
    succeed Abstraction
        |. symbol "λ"
        |= parseChar
        |= parseAbstractionTail


parseAbstractionTail : Parser Term
parseAbstractionTail =
    oneOf
        [ succeed identity
            |. symbol "."
            |= lazy (\_ -> parseTerm)
        , parseShorthandAbstraction
        ]


parseShorthandAbstraction : Parser Term
parseShorthandAbstraction =
    succeed Abstraction
        |= parseChar
        |= lazy (\_ -> parseAbstractionTail)


parseGroup : Parser Term
parseGroup =
    succeed Group
        |. symbol "("
        |= lazy (\_ -> parseTerm)
        |. symbol ")"


parseVariable : Parser Term
parseVariable =
    succeed Variable
        |= parseChar


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
