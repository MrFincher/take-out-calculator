module Price exposing (..)
import Bootstrap.Table as Table
import Html exposing (..)
import Html.Attributes exposing (..)

import Util exposing (..)

type Price = Price Int

addPrice : Price -> Price -> Price
addPrice (Price a) (Price b) = Price (a+b)

type alias DiscountedPrice =
    { originalPrice : Price
    , currentPrice : Price
    }

emptyDiscountedPrice : DiscountedPrice
emptyDiscountedPrice = DiscountedPrice (Price 0) (Price 0)

fromPrice : Price -> DiscountedPrice
fromPrice p = DiscountedPrice p p

discount : Float -> DiscountedPrice -> DiscountedPrice
discount p dp =
    let (Price cents) = dp.currentPrice
        withoutTax = toFloat cents / 1.07
        factor = p / 100
        saving = withoutTax * factor
    in {dp | currentPrice = toFloat cents - saving |> truncate |> Price}

isDiscounted : DiscountedPrice -> Bool
isDiscounted p = p.originalPrice /= p.currentPrice 

sumDiscountedPrices : List DiscountedPrice -> DiscountedPrice
sumDiscountedPrices = List.foldl addDiscountedPrice emptyDiscountedPrice

addDiscountedPrice : DiscountedPrice -> DiscountedPrice -> DiscountedPrice
addDiscountedPrice p1 p2  = DiscountedPrice
    (addPrice p1.originalPrice p2.originalPrice)
    (addPrice p1.currentPrice p2.currentPrice)

viewPriceTd : List (Attribute msg) -> DiscountedPrice -> Table.Cell msg
viewPriceTd atts price = tdNoBorder 
    [
        Table.cellAttr <| style "text-align" "right"
        ,Table.cellAttr <| style "width"
                <| bool (isDiscounted price) "15%" "30%"
    ]
    [
        div (class "row" :: atts)
            [ 
                if isDiscounted price
                    then span [] []
                    else Html.span (class "col" :: discounted)
                        [priceToStr price.originalPrice |> text]
                , span [class "col"] [priceToStr price.currentPrice |> text]
            ]
    ]

discounted : List (Html.Attribute msg)
discounted = [ style "color" "grey", style "text-decoration" "line-through"]


priceToStr : Price -> String
priceToStr (Price cents) =
    let euros = cents // 100
        rest = modBy 100 cents
        restStr = pad "0" 2 <| String.fromInt rest
    in "€" ++ String.fromInt euros ++ "." ++ restStr

