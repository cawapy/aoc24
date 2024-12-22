#load "Day22.fsx"
open System.Collections.Generic
open Secret

type ChangeSequence = int * int * int * int

let price (secret: secretT) : int =
    int (secret % secretT 10)

let nextDeltas (oldSecret: secretT, oldPrice: int, (_, d3, d2, d1): ChangeSequence) : secretT * int * ChangeSequence =
    let newSecret = nextSecret oldSecret
    let newPrice = price newSecret
    let change = newPrice - oldPrice
    let changeSequence = (d3, d2, d1, change)
    (newSecret, newPrice, changeSequence)

let initDeltas (s0: secretT) : secretT * int * ChangeSequence =
    (s0, price s0, (0, 0, 0, 0)) |> nextDeltas |> nextDeltas |> nextDeltas |> nextDeltas

let calculateMaxBananas (allBuyersInitialSecrets: secretT list) : int * ChangeSequence =

    let sequencesToBananas = Dictionary<ChangeSequence, int>()

    let processBuyer (buyersInitialSecret: secretT) : unit =
        let rec _processBuyer (previous: secretT * int * ChangeSequence) (encounteredSequences: HashSet<ChangeSequence>) (count: int) : unit =
            if count = 0 then ()
            else
                let newSecret, newPrice, newChangeSequence = nextDeltas previous
                if encounteredSequences.Add(newChangeSequence) then
                    sequencesToBananas[newChangeSequence] <-
                        newPrice + match sequencesToBananas.TryGetValue(newChangeSequence) with | true, p -> p | false, _ -> 0
                _processBuyer (newSecret, newPrice, newChangeSequence) encounteredSequences (count-1)
        _processBuyer (buyersInitialSecret |> initDeltas) (HashSet<ChangeSequence>()) (2000 - 4)

    allBuyersInitialSecrets |> Seq.iter processBuyer

    let changeSequenceWithMaxBananaYield = sequencesToBananas.Keys |> Seq.maxBy (fun k -> sequencesToBananas[k])
    let maxBananaYield = sequencesToBananas[changeSequenceWithMaxBananaYield]
    (maxBananaYield, changeSequenceWithMaxBananaYield)

let solvePuzzle (lines: string list) : unit =
    let secrets = lines |> List.map parseSecret
    let bananas, sequence = calculateMaxBananas secrets
    printfn $"Change sequence with max banana yield is {sequence}, it yields {bananas} bananas"

let samples = [
    "1";
    "2";
    "3";
    "2024";
]
#load "TextInput/Reader.fsx"
solvePuzzle (TextInput.Reader.readInput { TextInput.Reader.readInputOptions with Default = samples })