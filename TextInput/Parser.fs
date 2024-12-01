namespace TextInput

open System

module Parser =

    let lineToInts (line: string) =
        line.Split(" ", StringSplitOptions.RemoveEmptyEntries) |> Array.toList
        |> List.map Int32.Parse

    let linesToInts (lines: string list) =
        lines |> List.map lineToInts