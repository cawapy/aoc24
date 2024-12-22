#load "Day22.fsx"
open Secret

let applyNTimes (f: 'a -> 'a) (n: int) : 'a -> 'a =
    let rec _applyNTimes (fn: 'a -> 'a) (n: int) : 'a -> 'a =
        if n = 0 then fn
        else _applyNTimes (fun a -> f (fn a)) (n - 1)
    _applyNTimes id n

let generate2000thSecret : secretT -> secretT =
    applyNTimes nextSecret 2000


let solvePuzzle (lines: string list) : unit =
    let initialSecrets = lines |> List.map parseSecret
    let results = initialSecrets |> List.map generate2000thSecret
    let sum = results |> List.sum
    printfn $"Sum is {sum}"


let samples = [
    "1";
    "10";
    "100";
    "2024";
]
#load "TextInput/Reader.fsx"
solvePuzzle (TextInput.Reader.readInput { TextInput.Reader.readInputOptions with Default = samples })