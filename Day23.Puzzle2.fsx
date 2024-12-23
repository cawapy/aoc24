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

let formatGroupPassword (group: Computer seq) : string =
    System.String.Join(",", group |> Seq.sort)

let solve (input: Connection list) : unit =
    let rec scanConnections (connections: Connection list)
                            (peers: Dictionary<Computer, Computer list>) (connectionSet: HashSet<Computer * Computer>) :
                            Dictionary<Computer, Computer list> * HashSet<Computer * Computer> =
        match connections with
        | (c1, c2) :: tail ->
            peers[c1] <- match peers.TryGetValue(c1) with | true, l -> c2 :: l | false, _ -> [c2]
            peers[c2] <- match peers.TryGetValue(c2) with | true, l -> c1 :: l | false, _ -> [c1]
            connectionSet.Add((c1,c2)) |> ignore
            connectionSet.Add((c2,c1)) |> ignore
            scanConnections tail peers connectionSet
        | [] -> (peers, connectionSet)
    let peers, connections = scanConnections input (Dictionary<Computer, Computer list>()) (HashSet<Computer * Computer>())

    let cache = Dictionary<string list, string list>()
    let rec findLargestGroup (group: Computer list) (newComputer: Computer): Computer list =
        let sortedGroup = (newComputer::group) |> List.sort
        match cache.TryGetValue(sortedGroup) with
        | true, res -> res
        | false, _ ->
            if group |> List.forall (fun groupMember -> connections.Contains((groupMember,newComputer))) then
                let newGroup = newComputer :: group
                let res = peers[newComputer] |> Seq.map (findLargestGroup newGroup) |> Seq.maxBy List.length
                cache[sortedGroup] <- res
                res
            else
                group
    let largestGroup = peers.Keys |> Seq.map (findLargestGroup []) |> Seq.maxBy List.length

    let groupPassword = formatGroupPassword largestGroup
    printfn $"The password for LAN party is {groupPassword}"
    ()

let sample = [ "kh-tc"; "qp-kh"; "de-cg"; "ka-co"; "yn-aq"; "qp-ub"; "cg-tb"; "vc-aq"
               "tb-ka"; "wh-tc"; "yn-cg"; "kh-ub"; "ta-co"; "de-co"; "tc-td"; "tb-wq"
               "wh-td"; "ta-ka"; "td-qp"; "aq-cg"; "wq-ub"; "ub-vc"; "de-ta"; "wq-aq"
               "wq-vc"; "wh-yn"; "ka-de"; "kh-ta"; "co-tc"; "wh-qp"; "tb-vc"; "td-yn" ]
let lines = Reader.readInput { Reader.readInputOptions with Default = sample }

let connections = parseConnections lines
solve connections