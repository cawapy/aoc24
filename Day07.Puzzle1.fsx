#load "TextInput/Reader.fsx"
open System
open TextInput

let input = Reader.readInput { Reader.readInputOptions with
                                   Default = [ "190: 10 19";
                                               "3267: 81 40 27";
                                               "83: 17 5";
                                               "156: 15 6";
                                               "7290: 6 8 6 15";
                                               "161011: 16 10 13";
                                               "192: 17 8 14";
                                               "21037: 9 7 18 13";
                                               "292: 11 6 16 20"; ] }


type Equation = { Result: int64; Operands: int list }

let parseEquation (line: string) =
    let components = line.Split(":")
    { Result = Int64.Parse components[0]
      Operands = components[1].Split(" ", StringSplitOptions.RemoveEmptyEntries)
                 |> Array.toList |> List.map Int32.Parse }

let equations = input |> List.map parseEquation

let isSolvable (eq: Equation) =
    let rec solveEquation (n: int) (operands: int list) acc =
        if acc > eq.Result then false
        else match operands with
                | x :: tail when n % 2 = 0 -> solveEquation (n / 2) tail (acc + int64 x)
                | x :: tail                -> solveEquation (n / 2) tail (acc * int64 x)
                | [] -> acc = eq.Result
    let number = pown 2 (eq.Operands.Length - 1)
    [ 0 .. number ] |> List.exists (fun n -> solveEquation n eq.Operands.Tail eq.Operands.Head)

let sum = equations |> Seq.where isSolvable |> Seq.sumBy _.Result

printfn $"Sum of true equations is {sum}"