#load "TextInput/Reader.fsx"
open TextInput

let lines = Reader.readInput { Reader.readInputOptions with
                                   Default =  [
                                        "RRRRIICCFF";
                                        "RRRRIICCCF";
                                        "VVRRRCCFFF";
                                        "VVRCCCJFFF";
                                        "VVVVCJJCFE";
                                        "VVIVCCJJEE";
                                        "VVIIICJJEE";
                                        "MIIIIIJJEE";
                                        "MIIISIJEEE";
                                        "MMMISSJEEE";
                                   ] }

type Map = char array array
type Pos = int * int
type Direction = N | S | W | E
type Region = char * Pos list * int

let input: Map = lines |> List.map _.ToCharArray() |> List.toArray



let getPlant (map: Map) (pos: Pos) =
    let x, y = pos
    if 0 <= y && y < map.Length && 0 <= x && x < map[y].Length then map[y][x]
    else ' '

let coordinates (twoDArray: 'a array array) =
    [0..twoDArray.Length-1] |> List.collect (fun y -> [0..twoDArray[y].Length-1] |> List.map (fun x -> (x,y)))

let rec countConnectedPieces (pieces: (Pos * Direction) list) =
    let countConnectedHorizontal1Row (xx: int list) =
        let rec numberOfConnectedRanges previous elements acc =
            match elements with
            | e :: tail when e = previous + 1 -> numberOfConnectedRanges e tail acc
            | e :: tail                       -> numberOfConnectedRanges e tail (acc + 1)
            | [] -> acc
        let sorted = xx |> List.sort
        numberOfConnectedRanges sorted.Head sorted.Tail 1
    let countConnectedHorizontally (positions: Pos list) =
        let rows = positions |> List.groupBy fst
        rows |> List.sumBy (fun (_, values) -> (countConnectedHorizontal1Row (values |> List.map snd)))
    pieces |> List.groupBy snd |> List.sumBy (fun (direction: Direction, posNDirections: (Pos * Direction) list) ->
        let allPositions: Pos list = match direction with
                                        | E | W -> posNDirections |> List.map (fun ((x, y), _) -> (x, y))
                                        | N | S -> posNDirections |> List.map (fun ((x, y), _) -> (y, x)) // swap X and Y and use horizontal logic
        let result = allPositions |> countConnectedHorizontally
        result
    )

let findRegion (map: Map) (pos: Pos) =
    let regionPlant = getPlant map pos
    let rec _findRegion (pos: Pos) (visited: Pos list) (region: Pos list) (allFences: (Pos * Direction) list) =
        if getPlant map pos <> regionPlant || List.contains pos visited then
            (visited, region, allFences)
        else
            let x, y = pos
            let west = ((x - 1), y)
            let north = (x, (y - 1))
            let east = ((x + 1), y)
            let south = (x, (y + 1))

            let sidesW = if getPlant map west <> regionPlant then (pos, W) :: allFences else allFences
            let sidesN = if getPlant map north <> regionPlant then (pos, N) :: sidesW else sidesW
            let sidesE = if getPlant map east <> regionPlant then (pos, E) :: sidesN else sidesN
            let sidesS = if getPlant map south <> regionPlant then (pos, S) :: sidesE else sidesE
            let westV, westR, westS = _findRegion west (pos :: visited) (pos :: region) sidesS
            let northV, northR, northS = _findRegion north westV westR westS
            let eastV, eastR, eastS = _findRegion east northV northR northS
            let southV, southR, southS = _findRegion south eastV eastR eastS
            (southV, southR, southS)
    let _, regionFields, sidePieces = _findRegion pos [] [] []
    let sideCount = countConnectedPieces sidePieces
    (regionPlant, regionFields, sideCount)

let printRegion (map: Map) (region: Region) =
    printfn "-----------------------------------"
    let plant, fields, _ = region
    for y in 0..map.Length-1 do
        for x in 0..map[y].Length-1 do
            if List.contains (x, y) fields then
                printf $"{plant}"
            else
                printf "."
        printfn ""


let findRegions (map: Map) =
    let rec _findRegions (coordinates: Pos list) (regions: Region list) =
        match coordinates with
        | pos :: otherCoordinates ->
            let currentRegion = findRegion map pos
            let _, fields, _ = currentRegion
            let remainingCoordinates = otherCoordinates |> List.filter (fun pos -> not (List.contains pos fields))
            _findRegions remainingCoordinates (currentRegion :: regions)
        | [] -> regions
    _findRegions (coordinates map) [] |> List.rev

let allRegions = findRegions input

let calculatePrice (region: Region) =
    let _, fields, fenceCount = region
    let fieldCount = (List.length fields)
    let price = fieldCount * fenceCount
    price

let totalPrice = allRegions |> List.sumBy calculatePrice
printfn $"Total price of fencing is {totalPrice}"