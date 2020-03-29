module Main exposing (main)
import Browser exposing (document,Document)
import Html exposing (Html, text,node)
import Html.Attributes as H exposing (..)
import Html.Events exposing (onInput)
import Bootstrap.CDN as CDN
import Bootstrap.Table as Table
import Bootstrap.Card as Card
import Bootstrap.Card.Block as CardBlock
import Bootstrap.Utilities.Spacing as Spacing
import Bootstrap.Form.Textarea as TA
import Bootstrap.Text as Text
import Html5.DragDrop as DragDrop

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
    { restaurant : String
    , deliveryFee : Price
    , groups : List (List Item)
    , percentage : Float
    , dragDrop : DragDrop.Model (Int,Int) Int
    }

type Msg
    = Pasted String
    | PercentageChange String
    | DragDrop (DragDrop.Msg (Int,Int) Int)

init : flags -> (Model,Cmd Msg)
init _ = (WaitingForPaste Nothing, Cmd.none)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case (msg,model) of
    (DragDrop ddMsg, WithReceipt r) -> 
        let
            ( newModel, result ) =
                DragDrop.update ddMsg r.dragDrop
        in
            (WithReceipt { r
                | dragDrop = newModel
                , groups = (Maybe.withDefault identity <| Maybe.map handleDrop result) r.groups
            }
            , Cmd.none)
    (PercentageChange str, WithReceipt r) -> 
        let newPercentage = Maybe.withDefault r.percentage <| String.toFloat str
        in (WithReceipt 
            { r
                | groups = updatePrices newPercentage r.groups
                , percentage = newPercentage
            }
            , Cmd.none)
    (Pasted str, _) ->
        case parseReceipt str of
            Err err -> log err (WaitingForPaste err.context, Cmd.none)
            Ok receipt -> (WithReceipt 
                { groups = List.map List.singleton receipt.items
                , percentage = 0
                , restaurant = receipt.restaurant
                , deliveryFee = receipt.deliveryFee
                , dragDrop = DragDrop.init 
                }
                , Cmd.none)
    _ -> log "can not happen" (model, Cmd.none)

handleDrop : ((Int,Int), Int, DragDrop.Position) -> List (List Item) -> List (List Item)
handleDrop ((g,i),ng,_) gs = case get2D g i gs of
    Just item -> updateInd g (removeItem i) <| updateInd ng (\xs -> Just <| xs ++ [item]) <| gs 
    Nothing -> gs

get2D : Int -> Int -> List (List a) -> Maybe a
get2D i j xs = Maybe.andThen (get j) (get i xs)

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
    _ -> xs -- can not happen

get : Int -> List a -> Maybe a 
get i xs = List.head <| List.drop i xs

updatePrices : Float -> List (List Item) -> List (List Item)
updatePrices percentage  = List.map (List.map (calcFinal percentage))

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
    [Card.view (Card.config [Card.attrs [Spacing.m3]]
        |> Card.header [style "font-size" "20px"] [text "Email Text einfügen"]
        |> Card.block [] [CardBlock.custom (
            TA.textarea 
                [TA.attrs [autofocus True]
                -- , TA.value testData
                , TA.rows 18
                , TA.onInput Pasted
                ]
            )])
    ,let str =
            case mErr of
                Just err -> "Problem beim Auslesen von: " ++ err
                Nothing -> "Problem beim Auslesen"
    in Card.view (Card.config [Card.outlineDanger, Card.attrs [Spacing.m2]]
        |> block [CardBlock.textColor Text.danger, CardBlock.attrs [class "text-center"]] (text str))
    ]


discounted : List (Html.Attribute Msg)
discounted = [ style "color" "grey", style "text-decoration" "line-through"]

viewReceipt : WithReceiptState -> List (Html Msg)
viewReceipt r = [Html.div [class "container"]
    [Card.view (Card.config [Card.attrs [Spacing.m5Lg, Spacing.mt3]]
        |> Card.header [] [text r.restaurant]
        |> List.foldl (<<) identity (List.indexedMap (\i -> block [CardBlock.attrs [Spacing.p0, Spacing.p2Sm]] << viewGroup i) r.groups)
        |> block [] (Html.div [class "text-center"] 
            [Html.span [style "padding-right" "15px"] [text "Cashback:"]
            , Html.input 
                [Html.Events.onInput PercentageChange
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
    ]]

logId a = log a a 

viewGroup : Int -> List Item -> Html Msg
viewGroup g items = Card.view
    (Card.config [Card.attrs (Spacing.m2 :: DragDrop.droppable DragDrop g)]
        |> block [] (viewItemTable g items)
        
    )

block : List (CardBlock.Option Msg) -> Html Msg -> Card.Config Msg -> Card.Config Msg
block a c = Card.block a [CardBlock.custom c]

viewItemTable :Int -> List Item -> Html Msg
viewItemTable g items = Table.table
    {options = [Table.hover, Table.attr <| Spacing.mb0]
    ,thead = Table.simpleThead []
    ,tbody = Table.tbody [] <|
        List.indexedMap (viewItem g) items ++
        [
            let oldTotal = priceToStr <| sumPrices <| List.map .price items
                finalTotal = priceToStr <| sumPrices <|List.map .finalPrice items
            in Table.tr []
                [
                    tdNoBorder [] []
                    ,tdNoBorder [] []
                    ,Table.td []
                        [
                            Html.div [class "row"]
                                [
                                    if oldTotal==finalTotal
                                        then Html.span [] []
                                        else Html.span (class "col" :: discounted ++ sumStyle) [text oldTotal]
                                    , Html.span (class "col" :: sumStyle) [text finalTotal]
                                ]
                        ]
                ]
        ]
    }

sumPrices : List Price -> Price
sumPrices = Price << List.sum << List.map (\p ->  let (Price c) = p in c)

sumStyle : List (Html.Attribute Msg)
sumStyle = 
    [style "font-weight" "bold"
    ,style "text-align" "right"
    ,style "font-size" "120%"]

    
viewItem : Int -> Int -> Item -> Table.Row Msg
viewItem g i item =
    Table.tr (List.map Table.rowAttr <| DragDrop.draggable DragDrop (g,i))
        [ tdNoBorder
            [Table.cellAttr <| style "width" "10%"]
            [String.fromInt item.amount ++ "x" |> text ]
        , tdNoBorder 
            []
            [text item.name]
        , tdNoBorder 
            [Table.cellAttr <| style "text-align" "right"
            ,Table.cellAttr <| style "width" <| if item.price == item.finalPrice then "15%" else "30%"
            ]
            [
                Html.div [class "row"]
                    [ 
                        if item.price == item.finalPrice then Html.span [] []
                            else Html.span (class "col" :: discounted) [priceToStr item.price |> text]
                    , Html.span [class "col"] [priceToStr item.finalPrice |> text]
                    ]
            ]
        ] 

tdNoBorder : List (Table.CellOption Msg) -> List (Html Msg) -> (Table.Cell Msg)
tdNoBorder atts = Table.td <| atts ++ [Table.cellAttr <| style "border-top" "none"]

priceToStr : Price -> String
priceToStr (Price cents) =
    let euros = cents // 100
        rest = modBy 100 cents
        restStr = pad "0" 2 <| String.fromInt rest
    in "€" ++ String.fromInt euros ++ "." ++ restStr

pad : String -> Int -> String -> String
pad p l str = String.concat (List.repeat (l - String.length str) p) ++ str

testData : String
testData = """
Verfolge Deine Bestellung

Burger King
 
1	Cheeseburger	€ 10,50
 
1	Fries	€ 3,90

1	Soda	€ 2,50
 
Lieferkosten	Gratis
Transaktionskosten	€ 0,00
 
 
Total	€ 14,30
 
Zahlungsmöglichkeiten	
Payment method image
"""