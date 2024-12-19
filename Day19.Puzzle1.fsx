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

let isDesignPossible (allTowels: Towel list) (design: Design) =
    let rec _isDesignPossible (currentTowels: Towel list) (design: Design) =
        if currentTowels.Length = 0 then false
        else
            match design with
            | designColor :: designTail ->
                let nextTowels = currentTowels |>
                                 List.collect (fun currentTowel ->
                                    match currentTowel with
                                    | towelColor :: towelTail when towelColor = designColor -> [towelTail]
                                    | _ :: _ -> []
                                    | [] -> allTowels |>
                                            List.where (fun t -> t.Head = designColor) |>
                                            List.map _.Tail
                    )
                _isDesignPossible (nextTowels |> List.distinct) designTail
            | [] -> true
    _isDesignPossible allTowels design

let countDesigns (input: Input) =
    input.Designs |> List.where (isDesignPossible input.Towels) |> List.length

let input = parse lines
let result = countDesigns input
printfn $"{result} designs are possible"