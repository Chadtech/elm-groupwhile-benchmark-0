module Main exposing (main)

import Benchmark exposing (Benchmark)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


main : BenchmarkProgram
main =
    program oldAndNewGroupWhile


oldAndNewGroupWhile : Benchmark
oldAndNewGroupWhile =
    List.range 0 4
        |> List.map benchmarkOldAndNewGroupWhileAtSize
        |> Benchmark.describe "Old and new groupWhile implementations"


benchmarkOldAndNewGroupWhileAtSize : Int -> Benchmark
benchmarkOldAndNewGroupWhileAtSize size =
    let
        scaledSize : Int
        scaledSize =
            5 ^ size

        title : String
        title =
            [ String.fromInt (scaledSize * 9)
            , " element lists"
            ]
                |> String.concat
    in
    Benchmark.compare
        title
        "Old groupWhile"
        (benchmarkOldGroupWhile scaledSize)
        "New groupWhile"
        (benchmarkNewGroupWhile scaledSize)


benchmarkOldGroupWhile : Int -> () -> List ( Int, List Int )
benchmarkOldGroupWhile size _ =
    [ 1, 2, 3, 2, 4, 1, 3, 2, 1 ]
        |> List.repeat size
        |> List.concat
        |> oldGroupWhile (<)


benchmarkNewGroupWhile : Int -> () -> List ( Int, List Int )
benchmarkNewGroupWhile size _ =
    [ 1, 2, 3, 2, 4, 1, 3, 2, 1 ]
        |> List.repeat size
        |> List.concat
        |> newGroupWhile (<)


newGroupWhile : (a -> a -> Bool) -> List a -> List ( a, List a )
newGroupWhile isSameGroup items =
    List.foldr
        (\x acc ->
            case acc of
                [] ->
                    [ ( x, [] ) ]

                ( y, restOfGroup ) :: groups ->
                    if isSameGroup x y then
                        ( x, y :: restOfGroup ) :: groups

                    else
                        ( x, [] ) :: acc
        )
        []
        items


oldGroupWhile : (a -> a -> Bool) -> List a -> List ( a, List a )
oldGroupWhile condition list =
    accumulateGroupWhile condition list []


accumulateGroupWhile : (a -> a -> Bool) -> List a -> List ( a, List a ) -> List ( a, List a )
accumulateGroupWhile condition list accum =
    case list of
        [] ->
            List.reverse accum

        first :: rest ->
            let
                ( thisGroup, ungroupedRest ) =
                    oneGroupWhileHelper condition first rest
            in
            accumulateGroupWhile
                condition
                ungroupedRest
                (( first, thisGroup ) :: accum)


oneGroupWhileHelper : (a -> a -> Bool) -> a -> List a -> ( List a, List a )
oneGroupWhileHelper condition first list =
    case list of
        [] ->
            ( [], [] )

        second :: rest ->
            if condition first second then
                let
                    ( thisGroup, ungroupedRest ) =
                        oneGroupWhileHelper condition second rest
                in
                ( second :: thisGroup, ungroupedRest )

            else
                ( [], list )
