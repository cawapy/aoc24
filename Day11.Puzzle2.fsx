#load "TextInput/Reader.fsx"
open System
open System.Collections.Generic
open System.IO

let sample = "125 17"

let readLines (reader: TextReader) =
    Seq.unfold (fun () ->
        match reader.ReadLine() with
        | null | "" -> None
        | s -> Some (s, ())
    ) ()

let processLine (line: string) =
    let input = line.Split(" ") |> Array.map Int64.Parse |> Array.toList

    let rec tenPowN n acc =
        if n = 0 then acc else tenPowN (n - 1) (acc * 10)
    let rec countDigits (n: int64) acc =
        if n = 0 then acc
        else countDigits (n / int64 10) (acc+1)
    let splitNumber (x: int64) =
        let digits = countDigits x 0
        if digits % 2 = 0 then
            let divider = int64 (tenPowN (digits / 2) 1)
            let left = (int64 x) / divider
            let right = x - (left * divider)
            Some (left, right)
        else None
    let cache = Dictionary<int64 * int, int64>()
    let rec processNumber (blinks: int) (n: int64) =
        if blinks = 0 then int64 1 else
            match cache.TryGetValue((n, blinks)) with
            | true, result -> result
            | _ ->
                let count = if n = 0 then processNumber (blinks-1) (int64 1)
                            else match splitNumber n with
                                    | Some (x, y) -> processNumber (blinks-1) (int64 x) + processNumber (blinks-1) (int64 y)
                                    | None -> processNumber (blinks-1) (int64 2024 * n)
                cache.Add((n, blinks), count)
                count

    let blinks = 75
    let numberOfStones = input |> List.sumBy (processNumber blinks)
    printfn $"Number of stones after {blinks} blinks is {numberOfStones}"

let lines = Seq.append [ sample ] (readLines Console.In)

lines |> Seq.iter processLine