#load "TextInput/Reader.fsx"
open System
open TextInput

let sample = [  "5,4"; "4,2"; "4,5"; "3,0"; "2,1";
                "6,3"; "2,4"; "1,5"; "0,6"; "3,3";
                "2,6"; "5,1"; "1,2"; "5,5"; "2,5";
                "6,5"; "1,4"; "0,4"; "6,4"; "1,1";
                "6,1"; "1,0"; "0,5"; "1,6"; "2,0"; ]
let lines = Reader.readInput { Reader.readInputOptions with Default = sample }

type Pos = int * int
type Map = char list list

let parse (input: string list) =
    input |>
        List.map _.Split(",") |>
        List.map (fun pair -> pair |> Array.map Int32.Parse) |>
        List.map (fun xy -> xy[0], xy[1])

let generateMap (len: int) (corrupted: Pos list) =
    [0..len-1] |>
        List.map (
            fun y ->
                [0..len-1] |>
                    List.map (
                        fun x ->
                            if List.contains (x,y) corrupted then '#' else '.'
                    )
        )

let printMap (map: Map) =
    map |> Seq.iter (fun row ->
        row |> Seq.iter (fun cell -> printf $"{cell}")
        printfn "")
    printfn ""

let findPath (listMap: Map) =
    let map = listMap |> List.map List.toArray |> List.toArray
    let endPoint = (map.Length - 1, map.Length - 1)
    let selectMin (left: int option) (right: int option) =
        match (left, right) with
        | None, None -> None
        | Some _, None -> left
        | None, Some _ -> right
        | Some l, Some r when l < r -> left
        | _ -> right
    let shortestLengths = map |> Array.map (fun row -> Array.replicate row.Length 0x7fffffff)
    let visitedArray = map |> Array.map (fun row -> Array.replicate row.Length false)
    let rec _findPaths (x: int, y: int) (dx: int, dy: int) (length: int) =
        if (x, y) = endPoint then Some length
        elif y < 0 || y >= map.Length || x < 0 || x >= map.Length then None
        elif map[y][x] <> '.' then None
        elif shortestLengths[y][x] <= length || visitedArray[y][x] then None
        else
            shortestLengths[y][x] <- length
            visitedArray[y][x] <- true
            let ret = (
                let straightOption = _findPaths (x+dx, y+dy) (dx, dy) (length+1)
                let leftOption = _findPaths (x+dy, y-dx) (dy, -dx) (length+1)
                let rightOption = _findPaths (x-dy, y+dx) (-dy, dx) (length+1)
                selectMin (selectMin straightOption leftOption) rightOption
            )
            visitedArray[y][x] <- false
            ret
    let result: int option = _findPaths (0, 0) (0, 1) 0
    result

let len = if lines.Length = 25 then 7 else 71
let input = parse lines

let binarySearchTransition (lower: int) (upper: int) (predicate: int -> bool) =
    let rec _binarySearchTransition (lower: int) (upper: int) =
        if lower = upper then failwith "lower = upper"
        if lower + 1 = upper then
            Some (lower-1, upper-1)
        else
            let test = (lower + upper) / 2
            if predicate test then
                _binarySearchTransition lower test
            else
                _binarySearchTransition test upper
    if upper <= lower then failwith "Upper = lower"
    if predicate lower || not (predicate upper) then
        None
    else
        _binarySearchTransition lower upper


let isBreakingPath (input: Pos list) (n: int) =
    let map = generateMap len (input |> Seq.take n |> Seq.toList)
    //printMap map
    match findPath map with
    | Some _ -> false
    | None -> true

match binarySearchTransition 0 input.Length (isBreakingPath input) with
| Some (_, upper) ->
    let x, y = input |> List.item upper
    printfn $"Pos of byte breaking path is {x},{y}"
| None -> printfn "Not found"