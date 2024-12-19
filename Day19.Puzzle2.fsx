#load "TextInput/Reader.fsx"
open System
open TextInput
let sample = [
    "r, wr, b, g, bwu, rb, gb, br";
    "";
    "brwrr";
    "bggr";
    "gbbr";
    "rrbgbr";
    "ubwu";
    "bwurrg";
    "brgr";
    "bbrgwb";
]

let lines = Reader.readInput { Reader.readInputOptions with Default = sample; EmptyLineTerminator = false }

type Color = W | U | B | R | G
type Towel = Color list
type Design = Color list
type Input = { Towels : Towel list; Designs: Design list  }

let parse (lines: string list) =
    let toColorList (sequence: string) =
        sequence.ToCharArray() |>
            Array.map (fun c ->
                    match c with
                    | 'w' -> W | 'u' -> U | 'b' -> B | 'r' -> R | 'g' -> G
                    | _ -> failwith $"Unknown color {c}"
            ) |> Array.toList
    { Towels = lines.Head.Split(",", StringSplitOptions.TrimEntries) |> Array.toList |> List.map toColorList
      Designs = lines.Tail.Tail |> List.map toColorList }

let getPossibleDesigns (allTowels: Towel list) (design: Design) : int64 =

    let combineEquivalent (allItems: ('a * int64) list) : ('a * int64) list =
        allItems |> List.groupBy fst |> List.map (fun (x, items) -> (x, (items |> List.sumBy snd)))

    let rec _getPossibleDesigns (currentTowels: (Towel * int64) list) (design: Design) =
        if currentTowels.Length = 0 then int64 0
        else
            match design with
            | designColor :: designTail ->
                let nextTowels = currentTowels |>
                                 List.collect (fun (currentTowel, paths: int64) ->
                                    match currentTowel with
                                    | towelColor :: towelTail when towelColor = designColor -> [(towelTail, paths)]
                                    | _ :: _ -> []
                                    | [] -> allTowels |>
                                            List.where (fun t -> t.Head = designColor) |>
                                            List.map (fun t -> (t.Tail, paths))
                    )
                _getPossibleDesigns (nextTowels |> combineEquivalent) designTail
            | [] -> currentTowels |> List.where (fun (t, _) -> t = []) |> List.sumBy snd
    _getPossibleDesigns (allTowels |> List.map (fun t -> (t, int64 1))) design

let countDesignOptions (input: Input) =
    input.Designs |> List.sumBy (getPossibleDesigns input.Towels)

let input = parse lines
let result: int64 = countDesignOptions input
printfn $"{result} options exist to create designs"