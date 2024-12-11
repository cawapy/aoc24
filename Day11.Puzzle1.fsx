#load "TextInput/Reader.fsx"
open System
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
    let processNumber (n: int64) =
        if n = 0 then [int64 1]
        else match splitNumber n with
                | Some (x, y) -> [x; y]
                | None -> [int64 2024 * n]
    let blink (elements: int64 list) =
        List.collect processNumber elements
    let apply (f: 'a -> 'a) (x: 'a) (n: int) =
        List.fold (fun x _ -> f x) x [1..n]
    let blinks = 25
    let stones = apply blink input blinks
    let numberOfStones = stones |> List.length
    printfn $"Number of stones after {blinks} blinks is {numberOfStones}"

let lines = Seq.append [ sample ] (readLines Console.In)

lines |> Seq.iter processLine