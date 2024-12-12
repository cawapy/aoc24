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

let input: char array array  = lines |> List.map _.ToCharArray() |> List.toArray

let getPlant (map: char array array) (pos: int * int) =
    let x, y = pos
    if 0 <= y && y < map.Length && 0 <= x && x < map[y].Length then map[y][x]
    else ' '

let coordinates (twoDArray: 'a array array) =
    [0..twoDArray.Length-1] |> List.collect (fun y -> [0..twoDArray[y].Length-1] |> List.map (fun x -> (x,y)))

type Region = char * (int * int) list * int

let findRegion (map: char array array) (pos: int * int) =
    let regionPlant = getPlant map pos
    let rec _findRegion (pos: int * int) (visited: (int * int) list) (region: (int * int) list) (allFences: int) =
        if getPlant map pos <> regionPlant || List.contains pos visited then
            (visited, region, allFences)
        else
            let x, y = pos
            let west = ((x - 1), y)
            let north = (x, (y - 1))
            let east = ((x + 1), y)
            let south = (x, (y + 1))

            let fences: int = [west; north; east; south] |> List.where (fun p -> getPlant map p <> regionPlant) |> List.length
            let westV, westR, westF = _findRegion west (pos :: visited) (pos :: region) (allFences + fences)
            let northV, northR, northF = _findRegion north westV westR westF
            let eastV, eastR, eastF = _findRegion east northV northR northF
            let southV, southR, southF = _findRegion south eastV eastR eastF
            (southV, southR, southF)
    let _, regionFields, fenceCount = _findRegion pos [] [] 0
    (regionPlant, regionFields, fenceCount)

let printRegion (map: char array array) (region: Region) =
    printfn "-----------------------------------"
    let plant, fields, _ = region
    for y in 0..map.Length-1 do
        for x in 0..map[y].Length-1 do
            if List.contains (x, y) fields then
                printf $"{plant}"
            else
                printf "."
        printfn ""


let findRegions (map: char array array) =
    let rec _findRegions (coordinates: (int * int) list) (regions: Region list) =
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