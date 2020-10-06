module Main exposing (main)

import Browser
import Html exposing (Html, div, fieldset, form, h1, nav, span, text)
import Html.Attributes exposing (class)
import Html.Attributes.Extra
import Html.Events exposing (onClick)
import Maybe.Extra
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


isCustomer : User -> Bool
isCustomer user =
    case user of
        Customer_ _ ->
            True

        Vendor_ _ ->
            False


isVendor : User -> Bool
isVendor user =
    case user of
        Customer_ _ ->
            False

        Vendor_ _ ->
            True


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
    | ProductPrice (Maybe Int)
    | ProductQuantity (Maybe Int)


type alias Model =
    { userForm : User
    , request : WebData ()
    }


defaultModel : Model
defaultModel =
    { userForm = Customer_ defaultCustomer
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
view { userForm } =
    { title = "Registration"
    , body =
        [ div [ class "flex flex-col items-center justify-center w-full max-w-4xl m-auto text-2xl min-h-screen-6xl space-y-4" ]
            [ nav [ class "flex flex-row items-center w-full p-4 pb-8 border-0 border-b-8 border-blue-900 space-x-4" ]
                [ h1 [ class "flex-grow text-6xl font-bold" ] [ text "Sign Up" ]
                , button
                    { msg = Just <| SetUserType CustomerUser
                    , style =
                        if isCustomer userForm then
                            Primary

                        else
                            Secondary
                    }
                    [ text "Customer" ]
                , button
                    { msg = Just <| SetUserType VendorUser
                    , style =
                        if isVendor userForm then
                            Primary

                        else
                            Secondary
                    }
                    [ text "Vendor"
                    ]
                ]
            , form [ class "flex flex-col w-full p-4 space-y-8" ]
                [ case userForm of
                    Customer_ customer ->
                        customerForm customer

                    Vendor_ vendor ->
                        vendorform vendor
                , div [ class "flex flex-row justify-end" ] [ button { msg = Nothing, style = Primary } [ text "Submit" ] ]
                ]
            ]
        ]
    }


customerForm : Customer -> Html Msg
customerForm { email, firstName, lastName } =
    fieldset [ class "flex flex-col space-y-6" ]
        [ input
            { label = "Email"
            , value = email
            , onChange = SetCustomerField << CustomerEmail
            }
        , input
            { label = "First Name (Optional)"
            , value = firstName
            , onChange = SetCustomerField << FirstName
            }
        , input
            { label = "Last Name (Optional)"
            , value = lastName
            , onChange = SetCustomerField << LastName
            }
        ]


vendorform : Vendor -> Html Msg
vendorform { email, companyName, productName, productPrice, productQuantity } =
    fieldset [ class "flex flex-col space-y-6" ]
        [ input
            { label = "Email"
            , value = email
            , onChange = SetVendorField << VendorEmail
            }
        , input
            { label = "Company Name"
            , value = companyName
            , onChange = SetVendorField << CompanyName
            }
        , input
            { label = "Product Name"
            , value = productName
            , onChange = SetVendorField << ProductName
            }
        , input
            { label = "Product Price"
            , value = Maybe.Extra.unwrap "" String.fromInt productPrice
            , onChange = SetVendorField << ProductPrice << String.toInt
            }
        , input
            { label = "Product Quantity"
            , value = Maybe.Extra.unwrap "" String.fromInt productQuantity
            , onChange = SetVendorField << ProductQuantity << String.toInt
            }
        ]


type BtnStyle
    = Primary
    | Secondary


button : { msg : Maybe Msg, style : BtnStyle } -> List (Html Msg) -> Html Msg
button { msg, style } =
    Html.button
        (class "p-4 border-2 border-blue-900 rounded-sm"
            :: Maybe.Extra.unwrap
                -- if there is no msg then this btn is disabled
                [ class "text-gray-600 border-gray-600 cursor-not-allowed pointer-events-none"
                , case style of
                    Primary ->
                        class "bg-gray-200"

                    Secondary ->
                        Html.Attributes.Extra.empty
                ]
                (onClick
                    >> List.singleton
                    >> (::)
                        (case style of
                            Primary ->
                                class "text-white bg-blue-900"

                            Secondary ->
                                Html.Attributes.Extra.empty
                        )
                )
                msg
        )


input : { label : String, value : String, onChange : String -> Msg } -> Html Msg
input { label, value, onChange } =
    Html.label [ class "flex flex-col items-start" ]
        [ span [] [ text label ]
        , Html.input
            [ class "w-full p-2 border-2 border-blue-800 rounded-sm"
            , Html.Events.onInput onChange
            , Html.Attributes.value value
            ]
            []
        ]
