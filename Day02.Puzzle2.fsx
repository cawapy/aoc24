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

let getDampenedReports (report: int list) =
    let dampenAtPosition position =
        report[..position-1] @ report[position+1..]
    [ 0 .. report.Length-1 ] |> Seq.map dampenAtPosition

let isSafeReportAfterDampening (report: int list) =
    isSafeReport report || Seq.exists isSafeReport (getDampenedReports report)

let numberOfSafeReports = reports |> List.where isSafeReportAfterDampening |> List.length

printfn $"Number of safe reports after dampening: {numberOfSafeReports}"