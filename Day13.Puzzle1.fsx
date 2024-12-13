#load "TextInput/Reader.fsx"
open System
open TextInput

let lines = Reader.readInput { Reader.readInputOptions with
                                   EmptyLineTerminator = false
                                   Default = [
                                        "Button A: X+94, Y+34";
                                        "Button B: X+22, Y+67";
                                        "Prize: X=8400, Y=5400";
                                        "";
                                        "Button A: X+26, Y+66";
                                        "Button B: X+67, Y+21";
                                        "Prize: X=12748, Y=12176";
                                        "";
                                        "Button A: X+17, Y+86";
                                        "Button B: X+84, Y+37";
                                        "Prize: X=7870, Y=6450";
                                        "";
                                        "Button A: X+69, Y+23";
                                        "Button B: X+27, Y+71";
                                        "Prize: X=18641, Y=10279"
                                    ] }
type Machine = int * int * int * int * int * int

let parse (parseInput: string list) =
    let rec _parse (machines: Machine list) AX AY BX BY PX PY (parseLines: (string list * string) list) =
        match parseLines with
        | (["Button A"; "X"; x; "Y"; y], _) :: tail ->
            _parse machines (Int32.Parse x) (Int32.Parse y) 0 0 0 0 tail
        | (["Button B"; "X"; x; "Y"; y], _) :: tail ->
            _parse machines AX AY (Int32.Parse x) (Int32.Parse y) 0 0 tail
        | (["Prize";    "X"; x; "Y"; y], _) :: tail ->
            _parse machines AX AY BX BY (Int32.Parse x) (Int32.Parse y) tail
        | ([""], _) :: tail ->
            let newMachine : Machine = ( AX, AY, BX, BY, PX, PY )
            _parse (newMachine :: machines) 0 0 0 0 0 0 tail
        | (_, l) :: _ -> failwith $"Unexpected input %A{l}"
        | [] -> machines
    List.append parseInput [""] |>
        List.map (fun l -> (l.Split([|':'; ','; '+'; '='|], StringSplitOptions.TrimEntries) |> Array.toList, l)) |>
        _parse [] 0 0 0 0 0 0

let cost (machine: Machine) =
    let AX, AY, BX, BY, PX, PY = machine
    let bd = (double PY - double AY * double PX / double AX)/(double BY - double AY * double BX / double AX)
    let ad = double PX / double AX - double bd * double BX / double AX
    let a = int (round ad)
    let b = int (round bd)
    if a * AX + b * BX = PX && a * AY + b * BY = PY then 3 * a + b
    else 0

let machines = lines |> parse
let totalCost = machines |> List.sumBy cost
printfn $"Total cost is {totalCost}"