module Util exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Bootstrap.Table as Table
import Bootstrap.Card as Card
import Bootstrap.Card.Block as CardBlock

maybe : b -> (a -> b) -> Maybe a -> b
maybe d f = Maybe.withDefault d << Maybe.map f

bool : Bool -> a -> a -> a
bool c t f = if c then t else f

get : Int -> List a -> Maybe a 
get i xs = List.head <| List.drop i xs

get2D : Int -> Int -> List (List a) -> Maybe a
get2D i j xs = Maybe.andThen (get j) (get i xs)

removeItem : Int -> List a -> Maybe (List a)
removeItem i xs = case xs of 
    [_] -> Nothing
    xs_ -> Just <| deleteInd i xs_

deleteInd : Int -> List a -> List a
deleteInd i xs = List.take i xs ++ List.drop (i+1) xs

updateInd : Int -> (a -> Maybe a) -> List a -> List a
updateInd i f xs = case List.drop i xs of
    (x::rest) -> 
        let newItem = Maybe.withDefault [] (Maybe.map List.singleton (f x))
        in List.take i xs ++ newItem ++ rest
    _ -> xs -- invalid Ind

pad : String -> Int -> String -> String
pad p l str = String.concat (List.repeat (l - String.length str) p) ++ str

-- Logging

log : toLog -> a -> a
log toLog = Debug.log (Debug.toString toLog)

logId : a -> a
logId a = log a a 


-- Bootstrap

tdNoBorder : List (Table.CellOption msg) -> List (Html msg) -> (Table.Cell msg)
tdNoBorder atts = Table.td <| atts ++ [Table.cellAttr <| style "border-top" "none"]

block : List (CardBlock.Option msg) -> Html msg -> Card.Config msg -> Card.Config msg
block a c = Card.block a [CardBlock.custom c]
