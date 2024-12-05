#load "TextInput/Reader.fsx"
open System
open TextInput

let input = Reader.readLinesOrDefault Console.In [
    "MMMSXXMASM"
    "MSAMXMSMSA"
    "AMXSXMAAMM"
    "MSAMASMSMX"
    "XMASAMXAMM"
    "XXAMMXXAMA"
    "SMSMSASXSS"
    "SAXAMASAAA"
    "MAMMMXMMMM"
    "MXMXAXMASX"
]

let countWords (input: string list) (word: string) =
    let isMatch (pos: int * int) (direction: int * int) =
        let getChar (x: int) (y: int) =
            if 0 <= y && y < input.Length && 0 <= x && x < input[y].Length then
                input[y][x]
            else
                '_'
        let x, y = pos
        let dx, dy = direction
        [0 .. word.Length - 1] |> List.forall (fun i -> word[i] = getChar (x + i * dx) (y + i * dy) )
    let startPoints = seq { for y in 0 .. input.Length - 1 do
                            for x in 0 .. input[y].Length - 1 do
                            yield (x, y) }
    let directions = List.allPairs [ -1 .. 1 ] [ -1 .. 1 ] |> List.except [(0,0)]
    Seq.allPairs startPoints directions |> Seq.where (fun pair -> isMatch (fst pair) (snd pair)) |> Seq.length

let xmasCount (input: string list) =
    countWords input "XMAS"

let result = xmasCount input

printfn $"XMAS appears {result} times"