open System
open TextInput

let calculateDistanceSum (left: int list) (right: int list) =
    List.map2
        (fun l r -> abs (l - r))
        (left |> List.sort)
        (right |> List.sort )
    |> List.sum

let extractColumn columnIndex rows =
    List.map (List.item columnIndex) rows


let input = Reader.readLines Console.In |> Parser.linesToInts

let left = extractColumn 0 input
let right = extractColumn 1 input
let result = calculateDistanceSum left right

printfn $"Result: {result}"