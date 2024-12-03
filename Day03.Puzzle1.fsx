#load "TextInput/Reader.fsx"
open System
open System.Text.RegularExpressions
open TextInput

// HINT: there seems to be some limitations in Rider's integrated terminal regarding buffer size;
// use ordinary console or shell redirection to feed input
let lines = Reader.readLinesOrDefault Console.In [ "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))" ]

type Multiply = {
    x: int
    y: int
}

let parseLines (lines: string list) =
    let parseLine (line: string) =
        Regex(@"mul\((?<x>\d{1,3}),(?<y>\d{1,3})\)")
            .Matches line
            |> Seq.map (fun (m:Match) -> {
                x = Int32.Parse(m.Groups["x"].Value);
                y = Int32.Parse(m.Groups["y"].Value)
            })
            |> Seq.toList
    lines |> List.collect parseLine

let instructions = parseLines lines

let sum = instructions |> List.sumBy (fun m -> m.x * m.y)

printfn $"Sum of multiplication results: {sum}"