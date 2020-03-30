module Main exposing (main)
import Browser exposing (document,Document)
import Html exposing (Html, text,node)
import Html.Attributes as H exposing (..)
import Html.Events exposing (onInput)
import Bootstrap.CDN as CDN
import Bootstrap.Card as Card
import Bootstrap.Card.Block as CardBlock
import Bootstrap.Utilities.Spacing as Spacing
import Bootstrap.Form.Textarea as TA
import Bootstrap.Table as Table
import Bootstrap.Text as Text
import Html5.DragDrop as DragDrop

import Util exposing (..)
import Price exposing (..)
import Item exposing (..)
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
    | MainView MainViewState
    
type alias MainViewState =
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
    (DragDrop ddMsg, MainView r) -> 
        let (newDragDrop, result) = DragDrop.update ddMsg r.dragDrop
            updateGroups = maybe identity handleDrop result
            newModel = MainView
                { r
                    | dragDrop = newDragDrop
                    , groups = updateGroups r.groups
                }
            in (newModel, Cmd.none)

    (PercentageChange str, MainView r) -> case String.toFloat str of
        Nothing -> (model, Cmd.none)
        Just newPercentage -> 
            let newModel = MainView 
                    { r
                        | groups = updatePrices newPercentage r.groups
                        , percentage = newPercentage
                    }
            in (newModel, Cmd.none)
                
    (Pasted str, _) -> case parseReceipt str of
        Err err -> log err (WaitingForPaste err.context, Cmd.none)
        Ok receipt -> (MainView 
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
    Just item -> 
        updateInd g (removeItem i)
        <| updateInd ng (\xs -> Just <| xs ++ [item]) <| gs 
    Nothing -> gs

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

view : Model -> Document Msg
view model = 
    {title = "Cashback Calculator"
    ,body = CDN.stylesheet :: responsive :: case model of
        WaitingForPaste err -> viewPasteInput err
        MainView r -> viewMain r
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
        |> block [] (
            TA.textarea 
                [TA.attrs [autofocus True]
                -- , TA.value testData
                , TA.rows 18
                , TA.onInput Pasted
                ]
            )
        )
    , Card.view (Card.config [Card.outlineDanger, Card.attrs [Spacing.m2]]
        |> block [CardBlock.textColor Text.danger, CardBlock.attrs
            [class "text-center"]]
            (text <| "Problem beim Auslesen" ++ maybe "" (\s->" von: "++s) mErr)
        )
    ]

viewMain : MainViewState -> List (Html Msg)
viewMain r = [Html.div [class "container"]
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

viewGroup : Int -> List Item -> Html Msg
viewGroup g items = Card.view
    (Card.config [Card.attrs (Spacing.m2 :: DragDrop.droppable DragDrop g)]
        |> block [] (viewItemTable g items)
    )

viewItemTable :Int -> List Item -> Html Msg
viewItemTable g items = Table.table
    {options = [Table.hover, Table.attr <| Spacing.mb0]
    ,thead = Table.simpleThead []
    ,tbody = Table.tbody [] <|
        List.indexedMap (viewItem DragDrop g) items ++ [viewSum items]
    }

viewSum : List Item -> Table.Row Msg
viewSum items = Table.tr []
    [
        tdNoBorder [] []
        ,tdNoBorder [] []
        ,viewPriceTd sumStyle <| sumDiscountedPrices <| List.map .price items
    ]

sumStyle : List (Html.Attribute Msg)
sumStyle = 
    [style "font-weight" "bold"
    ,style "text-align" "right"
    ,style "font-size" "120%"]

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