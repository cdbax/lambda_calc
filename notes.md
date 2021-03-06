`run (succeed Bleh |. symbol "(" |. symbol "λ" |= (chompIf Char.isLower |> getChompedString)) "(λx"`

run : Parser a -> String -> Result (List DeadEnd) a

(succeed Bleh |. symbol "(" |. symbol "λ" |= (chompIf Char.isLower |> getChompedString)) <-- Parser Blah

Where Blah --> `type Blah = Bleh String`

`Bleh` is also a constructor with a signature like String -> Blah

Char.isLower : (Char -> Bool)

chompif : (Char -> Bool) -> Parser ()

chompIf Char.isLower : Parser ()

getChompedString : Parser a -> Parser String

(chompIf Char.isLower |> getChompedString) : Parser String

|= : Parser (a -> b) -> Parser a -> Parser b
a is String, so LHS must be Parser (String -> Blah)

|. : Parser keep -> Parser ignore -> Parser keep
keep has to be (String -> Blah)
succeed Bleh : Parser (String -> Blah)


