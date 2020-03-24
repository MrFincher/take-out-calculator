module Main exposing (main)
import Browser exposing (document,Document)
import Html exposing (Html, text,node)
import Html.Attributes as H exposing (..)
import Html.Events exposing (onInput,onSubmit)
import Bootstrap.CDN as CDN
import Bootstrap.Table as Table
import Bootstrap.Card as Card
import Bootstrap.Card.Block as CardBlock
import Bootstrap.Utilities.Spacing as Spacing
import Bootstrap.Form as Form
import Bootstrap.Form.Input as In
import Bootstrap.Form.Textarea as TA

import Types exposing (..)
import Parse exposing (parseReceipt)

main : Program () Model Msg
main = document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type Model
    = WaitingForPaste (Maybe String)
    | WithReceipt WithReceiptState
    
type alias WithReceiptState =
    {receipt : Receipt
    , percentage : Float
    }

type Msg
    = Pasted String
    | PercentageChange String

init : flags -> (Model,Cmd Msg)
init _ = (WaitingForPaste Nothing, Cmd.none)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case (msg,model) of
    (PercentageChange str, WithReceipt r) -> 
        let percentage = Maybe.withDefault 40 <| String.toFloat str
        in (WithReceipt {receipt = updatePrices percentage r.receipt, percentage = percentage} , Cmd.none)
    (Pasted str, _) ->
        case parseReceipt str of
            Err err -> log err (WaitingForPaste err.context, Cmd.none)
            Ok receipt -> (WithReceipt {receipt = updatePrices 40 receipt, percentage = 40}, Cmd.none)
    _ -> log "can not happen" (model, Cmd.none)

updatePrices : Float -> Receipt -> Receipt
updatePrices percentage receipt = {receipt | items = List.map (calcFinal percentage) receipt.items}

calcFinal : Float -> Item -> Item
calcFinal p i = 
    let (Price cents) = i.price
        withoutTax = toFloat cents / 1.07
        factor = p / 100
        saving = withoutTax * factor
    in {i | finalPrice = toFloat cents - saving |> truncate |> Price}

log : toLog -> a -> a
log toLog = Debug.log (Debug.toString toLog)

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

view : Model -> Document Msg
view model = 
    {title = "Cashback Calculator"
    ,body = CDN.stylesheet :: responsive :: case model of
        WaitingForPaste err -> viewPasteInput err
        WithReceipt r -> viewReceipt r
    }

responsive : Html Msg
responsive = node "meta" 
    [ name "viewport"
    , attribute "content" "width=device-width, initial-scale=1, shrink-to-fit=no"]
    []

viewPasteInput : Maybe String -> List (Html Msg)
viewPasteInput mErr = 
    [Card.view (Card.config [Card.attrs [Spacing.m2]]
        |> Card.header [style "font-size" "20px"] [text "Email Text einfügen"]
        |> Card.block [] [CardBlock.custom (
            TA.textarea 
                [TA.attrs [autofocus True]
                --, TA.value testData
                , TA.rows 18
                , TA.onInput Pasted
                ]
            )])
    ,let str =
            case mErr of
                Just err -> "Problem beim Auslesen von: " ++ err
                Nothing -> "Problem beim Auslesen"
    in Card.view (Card.config [Card.outlineDanger, Card.attrs [Spacing.m2]]
        |> block [CardBlock.attrs [style "color" "red", class "text-center"]] (text str))
    ]


discounted : List (Html.Attribute Msg)
discounted = [ style "color" "grey", style "text-decoration" "line-through"]

viewReceipt : WithReceiptState -> List (Html Msg)
viewReceipt r =
    [Card.view (Card.config [Card.attrs [Spacing.m2]]
        |> Card.header [] [text r.receipt.restaurant]
        |> block [] (viewItemTable r.receipt.items)
        |> block [] (Html.div [class "text-center"] 
            [Html.span [style "padding-right" "15px"] [text "Cashback:"]
            , Html.input 
                [Html.Events.onInput PercentageChange
                ,H.value (String.fromFloat r.percentage)
                ,type_ "number", H.step "5"
                ,H.min "0",  H.max "100"
                ,style "width" "65px"
                ,style "text-align" "right"
                ,style "display" "inline-block"
                ,class "form-control"
                ] []
            , Html.span [style "padding-left" "5px"] [text "%"]
            ])
        )
    ]

block : List (CardBlock.Option Msg) -> Html Msg -> Card.Config Msg -> Card.Config Msg
block a c = Card.block a [CardBlock.custom c]

viewItemTable : List Item -> Html Msg
viewItemTable items = Table.table
    {options = [Table.hover]
    ,thead = Table.simpleThead []
    ,tbody = Table.tbody [] (List.map viewItem items)
    }
    
viewItem : Item -> Table.Row Msg
viewItem item = Table.tr []
        [ Table.td [] [String.fromInt item.amount ++ "x" |> text ]
        , Table.td [] [text item.name]
        , Table.td [Table.cellAttr <| style "text-align" "right"]
        [
            Html.div [class "row"]
                [ Html.span (class "col" :: discounted) [priceToStr item.price |> text]
                , Html.span [class "col"] [priceToStr item.finalPrice |> text]
                ]
            ]
        ] 

priceToStr : Price -> String
priceToStr (Price cents) =
    let euros = cents // 100
        rest = modBy 100 cents
    in "€" ++ String.fromInt euros ++ "." ++ String.fromInt rest

testData : String
testData = """
Thuday
 
1	Cheeseburger	€ 10,50
 
1	Fries	€ 3,90
 
Lieferkosten	Gratis
Transaktionskosten	€ 0,00
 
 
Total	€ 14,30
 
Zahlungsmöglichkeiten	
Payment method image
"""