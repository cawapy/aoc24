module Secret

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