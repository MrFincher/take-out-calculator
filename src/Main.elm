module Main exposing (..)
import Browser exposing (document,Document)
import Html exposing (Html, ul, li, text, node)
import Html.Attributes exposing (autofocus,class,attribute,rel,href,style)
import Html.Events exposing (onInput)
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
    = WaitingForPaste
    | WithReceipt Receipt Float

type Msg
    = Pasted String
    | NewPercentage String

init : flags -> (Model,Cmd Msg)
init _ = (WaitingForPaste, Cmd.none)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case (msg,model) of
    (NewPercentage str, WithReceipt r _ ) -> case String.toFloat str of
        Just p -> (newPercentage r p, Cmd.none)
        -- TODO 
        Nothing -> log "could not parse percentage" (model, Cmd.none)
    (Pasted str, _) -> case parseReceipt str of
        Err err -> log err (model, Cmd.none)
        Ok receipt -> (newPercentage receipt 0.4, Cmd.none)
    _ -> log "can not happen" (model, Cmd.none)

newPercentage : Receipt -> Float -> Model
newPercentage receipt percentage = 
    let newItems = List.map (calcFinal percentage) receipt.items
    in WithReceipt {receipt | items = newItems} percentage

calcFinal : Float -> Item -> Item
calcFinal p i = 
    let (Price cents) = i.price
        withoutTax = toFloat cents / 1.07
    in {i | finalPrice = toFloat cents - withoutTax * p |> truncate |> Price}

log : toLog -> a -> a
log toLog = Debug.log (Debug.toString toLog)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

view : Model -> Document Msg
view model = 
    {title = "Cashback Calculator"
    ,body = bootstrap :: case model of
        WaitingForPaste -> viewPasteInput
        WithReceipt receipt precentage -> viewReceipt receipt precentage
    }

viewPasteInput : List (Html Msg)
viewPasteInput = 
    [Card.view (Card.config [Card.attrs [Spacing.m5]]
        |> Card.header [style "font-size" "20px"] [text "Email Text einfügen"]
        |> Card.block [] [CardBlock.custom (
            TA.textarea [TA.value testData, TA.rows 20, TA.onInput Pasted])])
    ]

discounted : List (Table.CellOption Msg)
discounted = List.map Table.cellAttr
    [ style "color" "grey", style "text-decoration" "line-through"]

viewReceipt : Receipt -> Float -> List (Html Msg)
viewReceipt receipt percentage =
    [Card.view (Card.config [Card.attrs [Spacing.m5]]
        |> Card.header [] [text receipt.restaurant]
        |> block [] (viewItemTable receipt.items)
        |> block [] (Form.formInline [class "justify-content-center"] 
            [Html.span [style "padding-right" "15px", class "text-center"] [text "Cashback Prozente:"]
            ,In.number 
                [In.onInput NewPercentage
                ,In.value (String.fromFloat percentage)
                , In.attrs [attribute "step" "0.1", style "width" "70px"]
                ]
            ])
        )
    ]


block a c = Card.block a [CardBlock.custom c]
viewItemTable : List Item -> Html Msg
viewItemTable items = Table.table
    {options = [Table.bordered, Table.hover]
    ,thead = Table.simpleThead []
    ,tbody = Table.tbody [] (List.map viewItem items)
    }
    
viewItem : Item -> Table.Row Msg
viewItem item = Table.tr []
        [ Table.td [] [String.fromInt item.amount |> text]
        , Table.td [] [text item.name]
        , Table.td discounted [priceToStr item.price |> text]
        , Table.td [] [priceToStr item.finalPrice |> text]
        ] 

priceToStr : Price -> String
priceToStr (Price cents) =
    let euros = cents // 100
        rest = modBy 100 cents
    in "€" ++ String.fromInt euros ++ "." ++ String.fromInt rest

bootstrap : Html Msg
bootstrap = node "link"
    [ rel "stylesheet"
    , href "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"
    , attribute "integrity" "sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T"
    , attribute "crossorigin" "anonymous"] []

testData = """
Thuday
 
1	[Vegetarisch] Dau Hu Xao Xa Ot - Jasminreis	€ 11,90
 
1	[Vegetarisch] Massamann Pak - Jasminreis	€ 9,90
 
1	[Gerichte mit Curry und Kokosmilch] Gaeng Keaw Wan Nua (sehr scharf)	€ 11,90
 
 
Lieferkosten	Gratis
Transaktionskosten	€ 0,00
 
 
Total	€ 33,70
 
Zahlungsmöglichkeiten	
Payment method image
Deine Bestellnummer	XQFWHM
"""