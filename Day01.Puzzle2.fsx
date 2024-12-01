#load "TextInput\Parser.fsx"
#load "TextInput\Reader.fsx"
open System
open TextInput

let similarityScore (left: int list) (right: int list) =
    let count searchList searchItem =
        searchList |> List.filter (fun x -> x = searchItem) |> List.length
    let rec _similarityScore (inputList: int list) acc =
        match inputList with
        | head :: tail -> _similarityScore tail acc + head * count right head
        | [] -> acc
    _similarityScore left 0

let rawInput = Reader.readLinesOrDefault Console.In [ "3 4"; "4 3"; "2 5"; "1 3"; "3 9"; "3 3"; ]
let input = rawInput |> Parser.linesToInts

let left = input |> List.map (List.item 0)
let right =  input |> List.map (List.item 1)

let result = similarityScore left right

printfn $"Similarity score {result}"