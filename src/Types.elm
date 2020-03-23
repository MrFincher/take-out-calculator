module Types exposing (..)

type alias Receipt =
    { restaurant : String
    , items : List Item
    , deliveryFee : Price
    }
type alias Item =
    { amount : Int
    , name : String
    , price : Price
    , finalPrice : Price
    }

type Price = Price Int