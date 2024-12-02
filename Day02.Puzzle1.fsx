#load "TextInput/Reader.fsx"
#load "TextInput/Parser.fsx"
open TextInput
open System

let lines = Reader.readLinesOrDefault Console.In [ "7 6 4 2 1"; "1 2 7 8 9"; "9 7 6 2 1"; "1 3 2 4 5"; "8 6 4 4 1"; "1 3 6 7 9" ]
let reports = lines |> Parser.linesToInts

let isSafeReport (report: int list) =
    let steps = Seq.map2 (fun x y -> x - y) report report.Tail
    Seq.forall (fun step -> 1 <= step && step <= 3) steps ||
    Seq.forall (fun step -> -3 <= step && step <= -1) steps

let numberOfSafeReports = reports |> List.where isSafeReport |> List.length

printfn $"Number of safe reports: {numberOfSafeReports}"