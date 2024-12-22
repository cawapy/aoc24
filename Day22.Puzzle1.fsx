#load "TextInput/Reader.fsx"
open TextInput
let samples = [
    "1";
    "10";
    "100";
    "2024";
]
let input = Reader.readInput { Reader.readInputOptions with Default = samples }
let initialSecrets = input |> List.map System.Int64.Parse

type secretT = int64
let secretT value : secretT = int64 value

let mix value secret : secretT =
    value ^^^ secret

let prune secret : secretT =
    secret % secretT 16777216

let nextSecret (secret: secretT) : secretT =
    let step1 (s: secretT) : secretT = prune (mix s (s * (secretT   64)))
    let step2 (s: secretT) : secretT = prune (mix s (s / (secretT   32)))
    let step3 (s: secretT) : secretT = prune (mix s (s * (secretT 2048)))
    step3 (step2 (step1 secret))

let applyNTimes (f: 'a -> 'a) (n: int) : 'a -> 'a =
    let rec _applyNTimes (fn: 'a -> 'a) (n: int) : 'a -> 'a =
        if n = 0 then fn
        else _applyNTimes (fun a -> f (fn a)) (n - 1)
    _applyNTimes id n

let generate2000thSecret = applyNTimes nextSecret 2000

let results = initialSecrets |> List.map generate2000thSecret

let sum = results |> List.sum
printfn $"Sum is {sum}"