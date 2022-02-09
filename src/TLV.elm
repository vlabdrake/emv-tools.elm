module TLV exposing (Object(..), decode)

import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder)
import List exposing (map, reverse, sum)


type Object
    = Primitive Int Bytes
    | Constructed Int (List Object)


isConstructed : Int -> Bool
isConstructed tag =
    let
        firstByte =
            if tag > 255 then
                tag // 256

            else
                tag
    in
    Bitwise.and firstByte 0x20 /= 0


decode : Bytes -> Maybe (List Object)
decode data =
    let
        len =
            Bytes.width data

        decoder =
            tlvDecoder len
    in
    Decode.decode decoder data


tagDecoder : Decoder Int
tagDecoder =
    let
        helper n =
            if Bitwise.and n 0x1F == 0x1F then
                Decode.unsignedInt8 |> Decode.andThen (\m -> Decode.succeed (n * 256 + m))

            else
                Decode.succeed n
    in
    Decode.unsignedInt8 |> Decode.andThen helper


lengthDecoder : Decoder Int
lengthDecoder =
    let
        helper n =
            if Bitwise.and n 0x80 /= 0 then
                case n - 0x80 of
                    1 ->
                        Decode.unsignedInt8

                    2 ->
                        Decode.unsignedInt16 BE

                    _ ->
                        Decode.fail

            else
                Decode.succeed n
    in
    Decode.unsignedInt8 |> Decode.andThen helper


primitiveValueDecoder : Decoder Bytes
primitiveValueDecoder =
    lengthDecoder |> Decode.andThen Decode.bytes


constructedValueDecoder : Decoder (List Object)
constructedValueDecoder =
    lengthDecoder |> Decode.andThen tlvDecoder


objectDecoder : Decoder Object
objectDecoder =
    let
        helper tag =
            if isConstructed tag then
                Decode.map (Constructed tag) constructedValueDecoder

            else
                Decode.map (Primitive tag) primitiveValueDecoder
    in
    tagDecoder |> Decode.andThen helper


tagLength : Int -> Int
tagLength tag =
    if tag < 256 then
        1

    else
        2


lenLength : Int -> Int
lenLength len =
    if len < 128 then
        1

    else if len < 256 then
        2

    else
        3


objectLength : Object -> Int
objectLength o =
    case o of
        Primitive tag value ->
            let
                valLength =
                    Bytes.width value
            in
            tagLength tag + lenLength valLength + valLength

        Constructed tag objs ->
            let
                valLength =
                    sum (map objectLength objs)
            in
            tagLength tag + lenLength valLength + valLength


tlvDecoder : Int -> Decoder (List Object)
tlvDecoder len =
    Decode.loop ( len, [] ) tlvStep


type alias State =
    ( Int, List Object )


tlvStep : State -> Decoder (Decode.Step State (List Object))
tlvStep ( len, tlv ) =
    if len > 0 then
        Decode.map (\x -> Decode.Loop ( len - objectLength x, x :: tlv )) objectDecoder

    else if len == 0 then
        reverse tlv |> Decode.Done |> Decode.succeed

    else
        Decode.fail
