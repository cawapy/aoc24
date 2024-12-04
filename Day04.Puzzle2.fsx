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

let countXmasCrossWords (input: string list) =
    let isCross (pos: int * int) =
        let getChar (x: int) (y: int) =
            if 0 <= y && y < input.Length && 0 <= x && x < input[y].Length then
                input[y][x]
            else
                '_'
        let x, y = pos
        if getChar x y = 'A' then
            let diag1 = (getChar (x - 1) (y - 1), getChar (x + 1) (y + 1))
            let diag2 = (getChar (x - 1) (y + 1), getChar (x + 1) (y - 1))
            (diag1 = ('M', 'S') || diag1 = ('S', 'M')) && (diag2 = ('M', 'S') || diag2 = ('S', 'M'))
        else
            false
    let startPoints = [ 1 .. input.Length - 2 ]
                    |> List.collect (fun y -> [ 1 .. input[y].Length - 2 ] |> List.map (fun x -> (x, y) ))
    startPoints |> List.where isCross |> List.length

let result = countXmasCrossWords input

printfn $"XMAS appears {result} times"