module LambdaReduce exposing (ReductionStep, Strategy(..), β_reduction)

import LambdaParse exposing (Term(..), termToString)



{-
   Reduction steps needs to be a more complex data structure than this.
   Keeping the end use-case in mind, for each step, we want to know what to render,
   and what to animate in as a substitution, so a list of end-results isn't really good enough.

   We want to know: Initial State, Target(s) - things being replaced, Substitutions - things doing the replacing.
   This probably isn't sufficient for the animation.
   The animation needs to deal w/ Strings. It needs to know:
   a) which parts of the string to highlight for the 2 terms being applied
   b) which abstraction is being animated for the replacement
   c) anything else that's changing (such as removing parens)

   The animation step probably only cares about the whole string, and the indices of things being moved around.
   The resultant string is probably helpful too, but not strictly necessary.

   I wonder if it should just work with Strings...probably makes sense to abstract away the Terms here.
-}


type alias Substring =
    { start : Integer
    , end : Integer
    }


type alias AnimationStep =
    { before : String
    , leftTerm : Maybe Substring
    , rightTerm : Maybe Substring
    , variableIndex : Maybe Integer
    , replacementIndices : List Integer
    , after : String
    }

type Animation
  = RemoveParens {start: String, firstIndex: Integer, secondIndex: Integer}
  | IdentifyAppliedTerm {start: String, termIndexStart: Integer, termIndexEnd: Integer}
  | IdentifyTargetTerms {start: String, terms: List {termIndexStart: Integer, termIndexEnd: Integer}}
  | DoApplication 


{-
   Normal order evaluation = repeatedly evaluating the leftmost **outermost** reducible expression (redex).
   Applicative order evaluation = repeatedly evaluating the leftmost **innermost** redex.
-}


type Strategy
    = Normal
    | Applicative



{-
   This is a PoC implementation that seems to do a single reduction.
   We need a way of identifying the term that will be replaced, and the term
   that is used to do the replacing, so the SVG animation logic can easily use
   the different parts that need to be highlighted or moved around.
-}


β_reduction : Strategy -> Term -> Term
β_reduction strategy term =
    let
        startReduce : Term -> Term
        startReduce startTerm =
            case startTerm of
                Application left right ->
                    let
                        result =
                            normalReduce left Nothing right
                    in
                    if result == startTerm then
                        -- We've tried reducing this application, and failed, so see if there's an
                        -- inner application we can reduce instead.
                        Application (startReduce left) right

                    else
                        result

                Group t ->
                    t

                otherTerm ->
                    otherTerm

        normalReduce : Term -> Maybe Char -> Term -> Term
        normalReduce startTerm boundVar appliedTerm =
            case startTerm of
                Group t ->
                    Group <| normalReduce t boundVar appliedTerm

                Abstraction char expr ->
                    case boundVar of
                        Nothing ->
                            -- Nothing bound...so bind it, because this must be the first Abstraction
                            normalReduce expr (Just char) appliedTerm

                        Just target ->
                            if char == target then
                                --Rebind! We can't replace anything in here, so just return everything from here down as found.
                                Abstraction char expr

                            else
                                -- No rebind, so just keep processing the expression
                                normalReduce expr boundVar appliedTerm

                Variable v ->
                    case boundVar of
                        Nothing ->
                            Variable v

                        Just target ->
                            if v == target then
                                appliedTerm

                            else
                                startTerm

                Application left right ->
                    case boundVar of
                        Nothing ->
                            -- Nothing bound, so just rebuild the original Application
                            -- This will allow us to try reducing the inner term
                            Application startTerm appliedTerm

                        Just _ ->
                            -- Bound value, so try applying in both terms, and return results as an Application
                            Application (normalReduce left boundVar appliedTerm) (normalReduce right boundVar appliedTerm)
    in
    case strategy of
        Normal ->
            startReduce term

        --                    ReductionStep term term (Abstraction 'x' (Variable 'x')) term
        Applicative ->
            term



--            ReductionStep term term (Abstraction 'x' (Variable 'x')) term
