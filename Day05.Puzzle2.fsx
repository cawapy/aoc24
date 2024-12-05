#load "TextInput/Reader.fsx"
#load "Day05.fsx"

open TextInput
open UpdateRules.Day05

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

let rules, updates = splitRulesAndUpdates input

let fixUpdate (rules: Rule list) (update: Update) =
    let comparePages (x: int) (y: int) =
        if pagePairViolatesRule rules (x, y) then 1 elif pagePairMatchesRule rules (x, y) then -1 else 0
    { Pages = List.sortWith comparePages update.Pages }

let nonCompliantUpdates = updates |> List.where (fun update -> not (isCompliantUpdate rules update))

let fixedUpdates = nonCompliantUpdates |> List.map (fixUpdate rules)
let result = sumCenterPagesOfUpdates fixedUpdates
printfn $"Sum of non-compliant but fixed updates' center page numbers: %A{result}"