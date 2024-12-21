#load "TextInput/Reader.fsx"
open TextInput

let sample = [
    "###############";
    "#...#...#.....#";
    "#.#.#.#.#.###.#";
    "#S#...#.#.#...#";
    "#######.#.#.###";
    "#######.#.#...#";
    "#######.#.###.#";
    "###..E#...#...#";
    "###.#######.###";
    "#...###...#...#";
    "#.#####.#.###.#";
    "#.#...#.#.#...#";
    "#.#.#.#.#.#.###";
    "#...#...#...###";
    "###############";
]

let lines = Reader.readInput { Reader.readInputOptions with Default = sample }

type TwoD<'a> = 'a[,]
type Map = TwoD<char>
type DistanceMap = TwoD<int>
type Pos = int * int
type Cheat = (Pos * Pos) * int
type CheatInput = (DistanceMap * DistanceMap) * int

let parse (lines: string list) : Map =
    Array2D.init lines.Length lines.Head.Length (fun x y -> lines[y][x])

let coordinates (input: TwoD<'a>) : Pos list =
    [0..input.GetLength(0) - 1] |>
        List.collect (fun y ->
            [0..input.GetLength(1) - 1] |>
                List.map (fun x -> (x, y))
            )

let trackPath (map: Map) : CheatInput =
    let findSinglePoint (symbol: char) : Pos =
        coordinates map |> List.where (fun (x, y) -> map[x,y] = symbol) |> List.head
    let rec track ((x, y): Pos) (length: int) (endSymbol: char) (dm: DistanceMap): int =
        if x < 0 || map.GetLength(0) <= x || y < 0 || map.GetLength(1) <= y || dm[x,y] <> -1 then 0x7fff_ffff
        else match map[x,y] with
                | e when e = endSymbol ->
                    dm[x,y] <- length
                    length
                | 'S' | '.' | 'E' ->
                    dm[x,y] <- length
                    min (min (track ((x+1), y) (length + 1) endSymbol dm) (track ((x-1), y) (length + 1) endSymbol dm))
                        (min (track (x, (y+1)) (length + 1) endSymbol dm) (track (x, (y-1)) (length + 1) endSymbol dm))
                | _ -> 0x7fff_ffff
    let fromStart : DistanceMap = Array2D.init (map.GetLength(0)) (map.GetLength(1)) (fun _ _ -> -1)
    let fromEnd : DistanceMap = Array2D.copy fromStart
    let len = track (findSinglePoint 'S') 0 'E' fromStart
    track (findSinglePoint 'E') 0 'S' fromEnd |> ignore
    ((fromStart, fromEnd), len)

let findCheats (((dmFwd, dmBwd), pathLength): CheatInput) : Cheat list =
    let maxLen = 20
    let lenX = dmFwd.GetLength 0
    let lenY = dmFwd.GetLength 1

    let trackCheats (startOnTrack: Pos) : Cheat list =
        let rec _generateCheats (cheatStart: Pos) (stepsStart: int) (cheatLength: int) (acc: Cheat list list): Cheat list =
            if cheatLength > maxLen then acc |> List.concat
            else
                let x, y = cheatStart
                let cl=cheatLength
                let diagonalEnds = [ 1 .. cl ] |>
                                        List.collect (fun i -> [(+i, +(cl-i));
                                                                (+i, -(cl-i));
                                                                (-i, +(cl-i));
                                                                (-i, -(cl-i))])
                let straightEnds = [(cl, 0); (-cl, 0); (0, cl); (0, -cl)]
                let cheatEnds = (diagonalEnds @ straightEnds) |>
                                    Seq.map (fun (dx, dy) -> (x + dx, y + dy)) |>
                                    Seq.where (fun (x, y) -> 0 <= x && x < lenX && 0 <= y && y < lenY) |>
                                    Seq.where (fun (x, y) -> dmBwd[x,y] <> -1)
                let cheats = cheatEnds |>
                                Seq.map (fun cheatEnd ->
                                        let remaining = dmBwd[fst cheatEnd, snd cheatEnd]
                                        let newLength = stepsStart + cheatLength + remaining
                                        let savings = pathLength - newLength
                                        ((cheatStart, cheatEnd), savings)
                                    ) |>
                                Seq.where (fun (_, s) -> s >= 50) |> Seq.toList
                _generateCheats cheatStart stepsStart (cheatLength+1) (cheats :: acc)
        let startSteps = dmFwd[fst startOnTrack,snd startOnTrack]
        _generateCheats startOnTrack startSteps 2 []
    let positions = coordinates dmFwd |>
                       List.where (fun (x, y) -> x > 0 && x < dmFwd.GetLength(0)-1 && y > 0 && y < dmFwd.GetLength(1)-1) |>
                       List.where (fun (x, y) -> dmFwd[x,y] <> -1)
    positions |>
        List.collect trackCheats |>
            List.distinctBy fst

let printArray (arr : 'a[,]) (fn: int -> int -> string) : unit =
    for y in 0..arr.GetLength(1)-1 do
        for x in 0..arr.GetLength(0)-1 do
            printf $"{fn x y}"
        printfn ""
    printfn ""

let map = parse lines
let cheatInfo = trackPath map
let cheats = findCheats cheatInfo


if map.GetLength(0) = 15 then
    let groups = cheats |> List.groupBy (fun (_, s) -> s) |> List.sortBy fst

    groups |> Seq.iter (fun g ->
        printfn $"There are {(snd g).Length} cheats that save {fst g} picoseconds"
    )
    printfn ""

let numberSavingAtLeast n = cheats |> Seq.where (fun (_, s) -> s >= n) |> Seq.length

printfn $"There are {numberSavingAtLeast 50} cheats that would save at least 50 ps"
printfn ""
printfn $"There are {numberSavingAtLeast 100} cheats that would save at least 100 ps"