module Main exposing (Model, Msg, main)

import Browser
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Encode exposing (encode, string, unsignedInt16, unsignedInt8)
import Hex.Convert
import Html exposing (Html, div, text, textarea, ul, li)
import Html.Attributes exposing (placeholder, style, value, class)
import Html.Events exposing (onInput)
import List
import String
import TLV



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { content : String
    }


init : Model
init =
    { content = "6F298407A0000000041010A51E500A4D6173746572436172648701015F2D047275656EBF0C059F4D020B0A" }



-- UPDATE


type Msg
    = Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newContent ->
            { model | content = newContent }



-- VIEW


view : Model -> Html Msg
view model =
    div [class "main"]
        [ textarea [ placeholder "List TLVObject in hex", value model.content, onInput Change ] []
        , div [] [ visualize (parse model.content) ]
        ]


parse : String -> List TLV.Object
parse hex =
    TLV.decode (hexToBytes (String.replace " " "" hex)) |> Maybe.withDefault []


hexToBytes : String -> Bytes
hexToBytes hex =
    case Hex.Convert.toBytes hex of
        Just data ->
            data

        Nothing ->
            encode (string "")


showTag : Int -> String
showTag tag =
    let
        encoder =
            if tag < 255 then
                unsignedInt8

            else
                unsignedInt16 BE
    in
    Hex.Convert.toString (encode (encoder tag))


visualize : List TLV.Object -> Html Msg
visualize tlv =
    ul [] (visualizeHelper tlv [])


visualizeHelper : List TLV.Object -> List (Html Msg) -> List (Html Msg)
visualizeHelper tlv acc =
    case tlv of
        obj :: objs ->
            case obj of
                TLV.Primitive tag value ->
                    visualizeHelper objs (li [] [ text (showTag tag ++ ": " ++ Hex.Convert.toString value) ] :: acc)

                TLV.Constructed tag children ->
                    visualizeHelper objs (li [] [ text (showTag tag ++ ": "), visualize children ] :: acc)

        [] ->
            List.reverse acc
