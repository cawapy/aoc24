#load "TextInput/Reader.fsx"
open System
open System.Text.RegularExpressions
open TextInput

// HINT: there seems to be some limitations in Rider's integrated terminal regarding buffer size;
// use ordinary console or shell redirection to feed input
let lines = Reader.readLinesOrDefault Console.In [ "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))" ]

type Multiply = {
    x: int
    y: int
}
type Instruction =
    | Mul of Multiply
    | Do
    | Dont

let parseLines (lines: string list) =
    let parseLine (line: string) =
        Regex(@"mul\((?<mulX>\d{1,3}),(?<mulY>\d{1,3})\)|(?<do>do\(\))|(?<dont>don't\(\))")
            .Matches line
            |> Seq.map (fun (m:Match) -> if m.Groups["mulX"].Success then
                                             Mul { x = Int32.Parse(m.Groups["mulX"].Value)
                                                   y = Int32.Parse(m.Groups["mulY"].Value) }
                                         elif m.Groups["do"].Success then
                                             Do
                                         elif m.Groups["dont"].Success then
                                             Dont
                                         else failwith "No matching instruction"
                )
            |> Seq.toList
    lines |> List.collect parseLine

let execute (instructions: Instruction list) =
    let rec _execute (instructions: Instruction list) enabled acc =
        match instructions with
        | [] -> acc
        | Do :: tail -> _execute tail true acc
        | Dont :: tail -> _execute tail false acc
        | Mul m :: tail -> _execute tail enabled ( if enabled then ( (m.x * m.y) :: acc ) else acc )
    _execute instructions true []

let instructions = lines |> parseLines
let values = instructions |> execute
let sum = values |> List.sum

printfn $"Sum of multiplication results: {sum}"