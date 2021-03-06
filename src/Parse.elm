module Parse exposing (parseReceipt)
import Parser exposing (Parser,Error, inContext, map, into, succeed, grab, ignore, separatedBy, followedBy, string, anyChar)
import Parser.Common exposing (int, tab, blank, newline)

import Price exposing (..)
import Item exposing (..)

type alias Receipt =
    { restaurant : String
    , items : List Item
    , deliveryFee : Price
    }

parseReceipt : String -> Result Error Receipt
parseReceipt str = Parser.parse str receipt

skipSpaces : Parser (List Char)
skipSpaces = Parser.zeroOrMore (Parser.oneOf [blank, newline])

receipt : Parser Receipt
receipt = into Receipt
    |> ignore (Parser.until (string "Verfolge Deine Bestellung") anyChar) |> ignore (string "Verfolge Deine Bestellung")
    |> ignore skipSpaces
    |> grab restaurant
    |> ignore skipSpaces
    |> grab (separatedBy1 skipSpaces item)
    |> ignore skipSpaces
    |> grab (string "Lieferkosten\t" |> followedBy price |> inContext "Lieferkosten")
    
restaurant : Parser String
restaurant = 
    Parser.until newline anyChar
    |> Parser.map String.fromList
    |> inContext "Restaurant" 

item : Parser Item
item = into Item
    |> grab int
    |> ignore tab
    |> grab (map String.fromList (Parser.until tab anyChar))
    |> ignore (Parser.oneOrMore tab)
    |> grab (Parser.map fromPrice <| price)
    |> inContext "Artikel Zeile"

price : Parser Price
price = inContext "Preis" <| Parser.oneOf
    [ string "Gratis" |> followedBy (Parser.succeed (Price 0))
    , Parser.string "€ " |> Parser.followedBy priceNum
    ]

priceNum : Parser Price
priceNum = into (++)
    |> grab (Parser.oneOrMore Parser.Common.digit)
    |> ignore (Parser.char ',')
    |> grab (Parser.repeat 2 Parser.Common.digit)
    |> Parser.map (String.fromList >> String.toInt)
    |> Parser.andThen (\res -> 
        case res of
            Just i -> succeed (Price i)
            Nothing -> Parser.fail "can not happen"
        )

separatedBy1 : Parser s -> Parser a -> Parser (List a)        
separatedBy1 s p = 
    s |> Parser.followedBy (separatedBy s p)
    |> Parser.maybe |> Parser.map (Maybe.withDefault [])
    |> Parser.map2 (::) p
