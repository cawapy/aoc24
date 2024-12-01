﻿open System
open TextInput

let totalDistanceSum (left: int list) (right: int list) =
    List.map2
        (fun l r -> abs (l - r))
        (left |> List.sort)
        (right |> List.sort )
    |> List.sum

let input = Reader.readLines Console.In |> Parser.linesToInts
let left = input |> List.map (List.item 0)
let right = input |> List.map (List.item 1)

let result = totalDistanceSum left right

printfn $"Total distance sum: {result}"