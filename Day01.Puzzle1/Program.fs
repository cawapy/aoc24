open System

let calculateDistanceSum (left: int list) (right: int list) =
    List.map2
        (fun l r -> abs (l - r))
        (left |> List.sort)
        (right |> List.sort )
    |> List.sum

let extractColumn columnIndex rows =
    List.map (List.item columnIndex) rows

let readLines (textReader: System.IO.TextReader) =
    let rec readLinesRec accumulator =
        let line = textReader.ReadLine()
        match line with
        | null | "" -> accumulator |> List.rev
        | _ -> readLinesRec (line :: accumulator)
    readLinesRec []

let parseLine (line: string) =
    line.Split(" ", StringSplitOptions.RemoveEmptyEntries) |> Array.toList
    |> List.map Int32.Parse

let lines = readLines Console.In
let input = lines |> List.map parseLine

let left = extractColumn 0 input
let right = extractColumn 1 input
let result = calculateDistanceSum left right

printfn $"Result: {result}"