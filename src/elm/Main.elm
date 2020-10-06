module Main exposing (main)

import Browser
import Html exposing (div, span, text)
import Html.Attributes exposing (class)
import RemoteData exposing (WebData)


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type UserType
    = CustomerUser
    | VendorUser


type User
    = Customer_ Customer
    | Vendor_ Vendor


type alias Customer =
    { email : String
    , firstName : String
    , lastName : String
    }


defaultCustomer : Customer
defaultCustomer =
    { email = ""
    , firstName = ""
    , lastName = ""
    }


type CustomerField
    = CustomerEmail String
    | FirstName String
    | LastName String


type alias Vendor =
    { email : String
    , companyName : String
    , productName : String
    , productPrice : Maybe Int
    , productQuantity : Maybe Int
    }


defaultVendor : Vendor
defaultVendor =
    { email = ""
    , companyName = ""
    , productName = ""
    , productPrice = Nothing
    , productQuantity = Nothing
    }


type VendorField
    = VendorEmail String
    | CompanyName String
    | ProductName String
    | ProductPrice Int
    | ProductQuantity Int


type alias Model =
    { form : User
    , request : WebData ()
    }


defaultModel : Model
defaultModel =
    { form = Customer_ defaultCustomer
    , request = RemoteData.NotAsked
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( defaultModel, Cmd.none )


type Msg
    = NoOp
    | SetUserType UserType
    | SetCustomerField CustomerField
    | SetVendorField VendorField
    | Submit
    | UserCreationUpdate (WebData ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Browser.Document Msg
view _ =
    { title = "Document Title"
    , body =
        [ div [ class "flex flex-col items-center justify-center min-h-screen text-6xl" ]
            [ span [] [ text "🎉" ]
            ]
        ]
    }
