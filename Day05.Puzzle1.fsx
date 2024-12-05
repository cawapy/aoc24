#load "TextInput/Reader.fsx"

open System
open TextInput

let input = Reader.readInput { Reader.readInputOptions with
                                  EmptyLineTerminator = false
                                  Default = [ "47|53"; "97|13"; "97|61"; "97|47"; "75|29"; "61|13"; "75|53";
                                              "29|13"; "97|29"; "53|29"; "61|53"; "97|53"; "61|29"; "47|13";
                                              "75|47"; "97|75"; "47|61"; "75|61"; "47|29"; "75|13"; "53|13";
                                              "";
                                              "75,47,61,53,29";
                                              "97,61,53,29,13";
                                              "75,29,13";
                                              "75,97,47,61,53";
                                              "61,13,29";
                                              "97,13,75,29,47";
                                            ] }

type Rule = { Before: int; After: int }
type Update = { Pages: int list }
let splitRulesAndUpdates (input: string list) =
    let parseRule (rule: string) =
        match rule.Split("|") with
        | [|x; y|] -> { Before = Int32.Parse(x); After = Int32.Parse(y) }
        | _ -> failwith "Error parsing rule"
    let parseUpdate (update: string) =
        { Pages = update.Split(",") |> Array.toList |> List.map Int32.Parse }
    let rec _splitRulesAndUpdates (input: string list) collectRules rulesAcc updatesAcc =
        match input with
        | "" :: tail -> _splitRulesAndUpdates tail false rulesAcc updatesAcc
        | x :: tail when collectRules -> _splitRulesAndUpdates tail collectRules ( x :: rulesAcc ) updatesAcc
        | x :: tail -> _splitRulesAndUpdates tail collectRules rulesAcc ( x :: updatesAcc )
        | [] -> ( rulesAcc |> List.rev |> List.map parseRule, updatesAcc |> List.rev |> List.map parseUpdate)
    _splitRulesAndUpdates input true [] []

let rules, updates = splitRulesAndUpdates input

let isCompliantUpdate (update: Update) (rules: Rule list)  =
    let pagePairViolatesRule (pair: int * int) =
        rules |> List.exists (fun rule -> rule = { Before = snd pair; After = fst pair })
    let pagesToCheck = [ 0..update.Pages.Length-2 ] |> List.collect (fun i -> [ i+1 .. update.Pages.Length-1 ] |> List.map (fun j -> (update.Pages[i], update.Pages[j])))
    pagesToCheck |> List.exists pagePairViolatesRule = false


let sumCenterPagesOfCompliantUpdates (updates: Update list) (rules: Rule list) =
    let compliantUpdates = updates |> List.where (fun update -> isCompliantUpdate update rules)
    compliantUpdates |> List.sumBy (fun u -> u.Pages[u.Pages.Length/2])

let result = sumCenterPagesOfCompliantUpdates updates rules
printfn $"Sum of compliant updates' center page numbers: %A{result}"