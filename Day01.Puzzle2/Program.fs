open System
open TextInput

let similarityScore (left: int list) (right: int list) =
    let count searchList searchItem =
        List.length (List.filter (fun x -> x = searchItem) searchList)
    let rec _similarityScore (inputList: int list) acc =
        match inputList with
        | head :: tail -> _similarityScore tail acc + head * count right head
        | [] -> acc
    _similarityScore left 0

let input = Reader.readLines Console.In |> Parser.linesToInts
// let input = [ [3; 4]; [4; 3]; [2; 5]; [1; 3]; [3; 9]; [3; 3] ]

let left = List.map (List.item 0) input
let right = List.map (List.item 1) input

let result = similarityScore left right

printfn $"Similarity score {result}"