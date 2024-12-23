module Secret

type secretT =
    int64
let secretT value : secretT =
    int64 value
let parseSecret : string -> secretT =
    System.Int64.Parse

let nextSecret (secret: secretT) : secretT =

    let mix sec value = sec ^^^ value
    let prune sec = sec % (secretT 16777216)

    let step1 sec = (sec * (secretT   64)) |> mix sec |> prune
    let step2 sec = (sec / (secretT   32)) |> mix sec |> prune
    let step3 sec = (sec * (secretT 2048)) |> mix sec |> prune

    secret |> step1 |> step2 |> step3