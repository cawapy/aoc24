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
    let concat (left: int64) (right: int) =
        let rec factor (result: int64) =
            if result > right then result else factor (result * int64 10)
        if right = 0 then left * int64 10 else left * (factor 1) + int64 right
    let rec solveEquation (n: int) (operands: int list) acc =
        if acc > eq.Result then false
        else match operands with
                | x :: tail when n % 3 = 0 -> solveEquation (n / 3) tail (acc + int64 x)
                | x :: tail when n % 3 = 1 -> solveEquation (n / 3) tail (acc * int64 x)
                | x :: tail                -> solveEquation (n / 3) tail (concat acc x)
                | [] -> acc = eq.Result
    let number = pown 3 (eq.Operands.Length - 1)
    [ 0 .. number ] |> List.exists (fun n -> solveEquation n eq.Operands.Tail eq.Operands.Head)

let sum = equations |> Seq.where isSolvable |> Seq.sumBy _.Result

printfn $"Sum of true equations is {sum}"