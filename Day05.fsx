namespace UpdateRules

open System

module Day05 =

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

    let isCompliantUpdate (update: Update) (rules: Rule list)  =
        let pagePairViolatesRule (pair: int * int) =
            rules |> List.exists (fun rule -> rule = { Before = snd pair; After = fst pair })
        let pagesToCheck = [ 0..update.Pages.Length-2 ] |> List.collect (fun i -> [ i+1 .. update.Pages.Length-1 ] |> List.map (fun j -> (update.Pages[i], update.Pages[j])))
        pagesToCheck |> List.exists pagePairViolatesRule = false

    let sumCenterPagesOfUpdates (updates: Update list)  =
        updates |> List.sumBy (fun u -> u.Pages[u.Pages.Length/2])