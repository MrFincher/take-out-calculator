module Item exposing (..)
import Bootstrap.Table as Table
import Html exposing (..)
import Html.Attributes exposing (..)
import Html5.DragDrop as DragDrop

import Util exposing (..)
import Price exposing (..)

type alias Item =
    { amount : Int
    , name : String
    , price : DiscountedPrice
    }

updatePrices : Float -> List (List Item) -> List (List Item)
updatePrices percentage  = List.map <| List.map <| updatePrice <| discount percentage

updatePrice : (DiscountedPrice -> DiscountedPrice) -> Item -> Item
updatePrice f item = {item | price = f item.price} 


viewItem : (DragDrop.Msg (Int,Int) Int -> msg) -> Int -> Int -> Item -> Table.Row msg
viewItem toMsg g i item =
    Table.tr (List.map Table.rowAttr <| DragDrop.draggable toMsg (g,i))
        [ tdNoBorder
            [Table.cellAttr <| style "width" "10%"]
            [String.fromInt item.amount ++ "x" |> text ]
        , tdNoBorder [] [text item.name]
        , viewPriceTd [] item.price
        ] 