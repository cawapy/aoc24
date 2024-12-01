#load "TextInput\Parser.fsx"
#load "TextInput\Reader.fsx"
open System
open TextInput

let totalDistanceSum (left: int list) (right: int list) =
    List.map2
        (fun l r -> abs (l - r))
        (left |> List.sort)
        (right |> List.sort )
    |> List.sum

let lines = Reader.readLinesOrDefault Console.In [ "3   4"; "4   3"; "2   5"; "1   3"; "3   9"; "3   3"; ]
let input = lines |> Parser.linesToInts

let left = input |> List.map (List.item 0)
let right = input |> List.map (List.item 1)

let result = totalDistanceSum left right

printfn $"Total distance sum: {result}"