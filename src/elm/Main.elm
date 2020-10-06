module Main exposing (main)

import Browser
import Html exposing (Html, div, fieldset, form, h1, nav, span, text)
import Html.Attributes exposing (class)
import Html.Attributes.Extra
import Html.Events exposing (onClick, onSubmit)
import Html.Extra
import Http
import Maybe.Extra
import RemoteData exposing (RemoteData)
import Result.Extra
import Validate exposing (Valid, Validator)


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


customerValidator : Validator ( CustomerField, String ) Customer
customerValidator =
    Validate.all
        [ Validate.ifInvalidEmail .email (\invalidEmail -> ( CustomerEmail invalidEmail, "Invalid Email" ))
        , Validate.ifFalse (.firstName >> String.all Char.isAlpha) ( FirstName "", "Only letters allowed (a-z and A-Z)" )
        , Validate.ifFalse (.lastName >> String.all Char.isAlpha) ( FirstName "", "Only letters allowed (a-z and A-Z)" )
        ]


updateCustomer : CustomerField -> Customer -> Customer
updateCustomer field customer =
    case field of
        CustomerEmail email ->
            { customer | email = email }

        FirstName firstName ->
            { customer | firstName = firstName }

        LastName lastName ->
            { customer | lastName = lastName }


type CustomerField
    = CustomerEmail String
    | FirstName String
    | LastName String


isCustomerEmail : CustomerField -> Bool
isCustomerEmail field =
    case field of
        CustomerEmail _ ->
            True

        _ ->
            False


isFirstName : CustomerField -> Bool
isFirstName field =
    case field of
        FirstName _ ->
            True

        _ ->
            False


isLastName : CustomerField -> Bool
isLastName field =
    case field of
        LastName _ ->
            True

        _ ->
            False


type alias Vendor =
    { email : String
    , companyName : String
    , productName : String
    , productPrice : Result String Float
    , productQuantity : Result String Int
    }


defaultVendor : Vendor
defaultVendor =
    { email = ""
    , companyName = ""
    , productName = ""
    , productPrice = Err ""
    , productQuantity = Err ""
    }


vendorValidator : Validator ( VendorField, String ) Vendor
vendorValidator =
    Validate.all
        [ Validate.ifInvalidEmail .email (\invalidEmail -> ( VendorEmail invalidEmail, "Invalid Email" ))
        , Validate.ifBlank .companyName ( CompanyName "", "Required" )
        , Validate.firstError
            [ Validate.ifBlank .productName ( ProductName "", "Required" )
            , Validate.ifFalse (.productName >> String.all Char.isAlphaNum) ( ProductName "", "Only letters and numbers are allowed (a-z, A-Z and 0-9)" )
            ]
        , Validate.firstError
            [ Validate.ifTrue (.productPrice >> (==) (Err "")) ( ProductPrice "", "Required" )
            , Validate.ifTrue (.productPrice >> Result.Extra.isErr) ( ProductPrice "", "Invalid Price" )
            ]
        , Validate.firstError
            [ Validate.ifTrue (.productQuantity >> (==) (Err "")) ( ProductQuantity "", "Required" )
            , Validate.ifTrue (.productQuantity >> Result.Extra.isErr) ( ProductQuantity "", "Invalid Quantity" )
            ]
        ]


updateVendor : VendorField -> Vendor -> Vendor
updateVendor field vendor =
    case field of
        VendorEmail email ->
            { vendor | email = email }

        CompanyName name ->
            { vendor | companyName = name }

        ProductName name ->
            { vendor | productName = name }

        ProductPrice price ->
            { vendor
                | productPrice =
                    -- NOTE this is a special case 'cause `String.toFloat "123." == Just 123`
                    -- without that particular test it would be impossible to input floating number
                    if String.endsWith "." price then
                        Err price

                    else
                        price |> String.toFloat |> Result.fromMaybe price
            }

        ProductQuantity qte ->
            { vendor | productQuantity = qte |> String.toInt |> Result.fromMaybe qte }


type VendorField
    = VendorEmail String
    | CompanyName String
    | ProductName String
    | ProductPrice String
    | ProductQuantity String


isVendorEmail : VendorField -> Bool
isVendorEmail field =
    case field of
        VendorEmail _ ->
            True

        _ ->
            False


isCompanyName : VendorField -> Bool
isCompanyName field =
    case field of
        CompanyName _ ->
            True

        _ ->
            False


isProductName : VendorField -> Bool
isProductName field =
    case field of
        ProductName _ ->
            True

        _ ->
            False


isProductPrice : VendorField -> Bool
isProductPrice field =
    case field of
        ProductPrice _ ->
            True

        _ ->
            False


isProductQuantity : VendorField -> Bool
isProductQuantity field =
    case field of
        ProductQuantity _ ->
            True

        _ ->
            False


type alias Model =
    { userForm : User
    , request : RemoteData Error ()
    }


type Error
    = ValidationError
    | HttpError Http.Error


defaultModel : UserType -> Model
defaultModel userType =
    { userForm =
        case userType of
            CustomerUser ->
                Customer_ defaultCustomer

            VendorUser ->
                Vendor_ defaultVendor
    , request = RemoteData.NotAsked
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( defaultModel CustomerUser, Cmd.none )


type Msg
    = SetUserType UserType
    | SetCustomerField CustomerField
    | SetVendorField VendorField
    | SubmitCustomer (Valid Customer)
    | SubmitVendor (Valid Vendor)
    | UserCreationUpdate (RemoteData Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetUserType userType ->
            ( defaultModel userType, Cmd.none )

        SetCustomerField customerField ->
            case model.userForm of
                Customer_ customer ->
                    ( { model
                        | userForm = Customer_ <| updateCustomer customerField customer
                        , request =
                            if model.request == RemoteData.Failure ValidationError then
                                RemoteData.Failure ValidationError

                            else
                                RemoteData.NotAsked
                      }
                    , Cmd.none
                    )

                Vendor_ _ ->
                    -- NOTE this should never happen by construction
                    ( model, Cmd.none )

        SetVendorField vendorField ->
            case model.userForm of
                Vendor_ vendor ->
                    ( { model
                        | userForm = Vendor_ <| updateVendor vendorField vendor
                        , request =
                            if model.request == RemoteData.Failure ValidationError then
                                RemoteData.Failure ValidationError

                            else
                                RemoteData.NotAsked
                      }
                    , Cmd.none
                    )

                Customer_ _ ->
                    -- NOTE this should never happen by construction
                    ( model, Cmd.none )

        SubmitCustomer _ ->
            Debug.todo "handle submit"

        SubmitVendor _ ->
            Debug.todo "handle submit"

        UserCreationUpdate newRequest ->
            ( { model | request = newRequest }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Browser.Document Msg
view { userForm, request } =
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
            , let
                ( submitFormAction, submitStyle ) =
                    case userForm of
                        Customer_ customer ->
                            Validate.validate customerValidator customer
                                |> Result.Extra.unwrap ( UserCreationUpdate <| RemoteData.Failure ValidationError, PrimaryDisabled )
                                    (\validCustomer -> ( SubmitCustomer validCustomer, Primary ))

                        Vendor_ vendor ->
                            Validate.validate vendorValidator vendor
                                |> Result.Extra.unwrap ( UserCreationUpdate <| RemoteData.Failure ValidationError, PrimaryDisabled )
                                    (\validVendor -> ( SubmitVendor validVendor, Primary ))
              in
              form
                [ class "flex flex-col w-full p-4 space-y-8"
                , onSubmit submitFormAction
                ]
                [ case userForm of
                    Customer_ customer ->
                        customerForm request customer

                    Vendor_ vendor ->
                        vendorform request vendor
                , div [ class "flex flex-row justify-end" ] [ button { msg = Just submitFormAction, style = submitStyle } [ text "Submit" ] ]
                ]
            ]
        ]
    }


customerForm : RemoteData Error () -> Customer -> Html Msg
customerForm request ({ email, firstName, lastName } as customer) =
    let
        errors =
            if request == RemoteData.Failure ValidationError then
                Validate.validate customerValidator customer
                    |> Result.Extra.error

            else
                Nothing

        feedbackForField test =
            errors
                |> Maybe.andThen
                    (List.filter (Tuple.first >> test)
                        >> List.head
                        >> Maybe.map Tuple.second
                    )
    in
    fieldset [ class "flex flex-col space-y-6" ]
        [ input
            { label = "Email"
            , feedback =
                feedbackForField isCustomerEmail
            , value = email
            , onChange = SetCustomerField << CustomerEmail
            }
        , input
            { label = "First Name (Optional)"
            , feedback =
                feedbackForField isFirstName
            , value = firstName
            , onChange = SetCustomerField << FirstName
            }
        , input
            { label = "Last Name (Optional)"
            , feedback =
                feedbackForField isLastName
            , value = lastName
            , onChange = SetCustomerField << LastName
            }
        ]


vendorform : RemoteData Error () -> Vendor -> Html Msg
vendorform request ({ email, companyName, productName, productPrice, productQuantity } as vendor) =
    let
        errors =
            if request == RemoteData.Failure ValidationError then
                Validate.validate vendorValidator vendor
                    |> Result.Extra.error

            else
                Nothing

        feedbackForField test =
            errors
                |> Maybe.andThen
                    (List.filter (Tuple.first >> test)
                        >> List.head
                        >> Maybe.map Tuple.second
                    )
    in
    fieldset [ class "flex flex-col space-y-6" ]
        [ input
            { label = "Email"
            , feedback = feedbackForField isVendorEmail
            , value = email
            , onChange = SetVendorField << VendorEmail
            }
        , input
            { label = "Company Name"
            , feedback = feedbackForField isCompanyName
            , value = companyName
            , onChange = SetVendorField << CompanyName
            }
        , input
            { label = "Product Name"
            , feedback = feedbackForField isProductName
            , value = productName
            , onChange = SetVendorField << ProductName
            }
        , input
            { label = "Product Price"
            , feedback = feedbackForField isProductPrice
            , value = Result.Extra.unpack identity String.fromFloat productPrice
            , onChange = SetVendorField << ProductPrice
            }
        , input
            { label = "Product Quantity"
            , feedback = feedbackForField isProductQuantity
            , value = Result.Extra.unpack identity String.fromInt productQuantity
            , onChange = SetVendorField << ProductQuantity
            }
        ]


type BtnStyle
    = Primary
    | PrimaryDisabled
    | Secondary


button : { msg : Maybe Msg, style : BtnStyle } -> List (Html Msg) -> Html Msg
button { msg, style } =
    Html.button
        [ class "p-4 border-2 border-blue-900 rounded-sm"
        , case style of
            Primary ->
                class "text-white bg-blue-900"

            PrimaryDisabled ->
                class "text-white bg-blue-200"

            Secondary ->
                Html.Attributes.Extra.empty
        , Maybe.Extra.unwrap
            Html.Attributes.Extra.empty
            onClick
            msg
        ]


input : { label : String, feedback : Maybe String, value : String, onChange : String -> Msg } -> Html Msg
input { label, feedback, value, onChange } =
    Html.label [ class "flex flex-col items-start" ]
        [ div [ class "flex flex-row space-x-8" ]
            [ span [] [ text label ]
            , Html.Extra.viewMaybe (\feedbackMsg -> span [ class "text-red-700" ] [ text feedbackMsg ]) feedback
            ]
        , Html.input
            [ class "w-full p-2 border-2 border-blue-800 rounded-sm"
            , Html.Attributes.Extra.attributeMaybe (\_ -> class "border-red-700") feedback
            , Html.Events.onInput onChange
            , Html.Attributes.value value
            ]
            []
        ]
