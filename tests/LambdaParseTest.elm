module LambdaParseTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import LambdaParse exposing (..)
import Parser exposing (run)
import Set exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "LambdaParse"
        [ describe "LambdaParse.parseTerm"
            [ test "parses variable" <|
                \_ ->
                    let
                        variable =
                            Ok (Variable 'x')
                    in
                    Expect.equal variable (run LambdaParse.parseTerm "x")
            , test "parses basic abstraction" <|
                \_ ->
                    let
                        expression =
                            Ok (Abstraction 'x' (Variable 'x'))
                    in
                    Expect.equal expression (run LambdaParse.parseTerm "λx.x")
            , test "parses True abstraction" <|
                \_ ->
                    let
                        expression =
                            Ok (Abstraction 'x' (Abstraction 'y' (Variable 'x')))
                    in
                    Expect.equal expression (run LambdaParse.parseTerm "λx.λy.x")
            , test "parses shortcut notation for True abstraction" <|
                \_ ->
                    let
                        expression =
                            Ok (Abstraction 'x' (Abstraction 'y' (Variable 'x')))
                    in
                    Expect.equal expression (run LambdaParse.parseTerm "λxy.x")
            , test "parses shortcut notation for pair abstraction" <|
                \_ ->
                    let
                        expression =
                            Ok (Abstraction 'x' (Abstraction 'y' (Abstraction 'z' (Application (Variable 'z') (Application (Variable 'x') (Variable 'y'))))))
                    in
                    Expect.equal expression (run LambdaParse.parseTerm "λxyz.zxy")
            , test "parses infinite loop" <|
                \_ ->
                    let
                        expression =
                            Ok (Application (Group (Abstraction 'x' (Application (Variable 'x') (Variable 'x')))) (Group (Abstraction 'x' (Application (Variable 'x') (Variable 'x')))))
                    in
                    Expect.equal expression (run LambdaParse.parseTerm "(λx.xx)(λx.xx)")
            ]
        ]
