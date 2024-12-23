#load "TextInput/Reader.fsx"
open System.Collections.Generic
open TextInput

type Computer = string
type Connection = Computer * Computer

let parseConnections (lines: string list) : Connection list =
    let parseConnection (i: int) (line: string) : Connection =
        match line.Split("-") with
        | [| c1; c2 |] -> (c1, c2)
        | _ -> failwith $"Could not understand input \'{line}\' in line nr. {i}"
    lines |> List.mapi parseConnection

let formatGroup (group: Computer seq) : string =
    System.String.Join(",", group)

let solve (input: Connection list) : unit =
    let map = Dictionary<Computer, Computer list>()
    let add (c1: Computer) (c2: Computer) =
        map[c1] <- match map.TryGetValue(c1) with | true, l -> c2 :: l | false, _ -> [c2]
    input |> Seq.iter (fun (c1, c2) -> add c1 c2; add c2 c1 )
    let findTriplet (c1: Computer) : Computer list list =
        let rec _find (ps: Computer list) (acc: Computer list list list) : Computer list list =
            match ps with
            | [] -> acc |> List.concat
            | c2 :: tail ->
                let newTriplets = tail |> List.collect (fun c3 -> if map[c2] |> List.contains c3 then [[c1; c2; c3] |> List.sort] else [])
                _find tail (newTriplets :: acc)
        _find map[c1] []
    let solution = map.Keys |> Seq.where (fun c -> c.StartsWith 't') |> Seq.collect findTriplet |> Seq.distinct |> Seq.sort |> Seq.toList
    //solution |> Seq.iter (fun g -> printfn $"{formatGroup g}")
    printfn $"There are {solution |> List.length} triplets including at least 1 computer starting with 't'"
    ()

let sample = [ "kh-tc"; "qp-kh"; "de-cg"; "ka-co"; "yn-aq"; "qp-ub"; "cg-tb"; "vc-aq"
               "tb-ka"; "wh-tc"; "yn-cg"; "kh-ub"; "ta-co"; "de-co"; "tc-td"; "tb-wq"
               "wh-td"; "ta-ka"; "td-qp"; "aq-cg"; "wq-ub"; "ub-vc"; "de-ta"; "wq-aq"
               "wq-vc"; "wh-yn"; "ka-de"; "kh-ta"; "co-tc"; "wh-qp"; "tb-vc"; "td-yn" ]
let lines = Reader.readInput { Reader.readInputOptions with Default = sample }

let connections = parseConnections lines
solve connections