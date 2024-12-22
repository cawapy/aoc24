type secretT =
    int64
let secretT value : secretT =
    int64 value
let parseSecret : string -> secretT =
    System.Int64.Parse

let nextSecret (secret: secretT) : secretT =
    let mix value (secret: secretT) : secretT =
        value ^^^ secret

    let prune (secret: secretT) : secretT =
        secret % secretT 16777216

    let step1 (s: secretT) : secretT = (s * (secretT   64)) |> mix s |> prune
    let step2 (s: secretT) : secretT = (s / (secretT   32)) |> mix s |> prune
    let step3 (s: secretT) : secretT = (s * (secretT 2048)) |> mix s |> prune

    secret |> step1 |> step2 |> step3

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