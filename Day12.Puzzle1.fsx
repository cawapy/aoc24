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

let getPlant (map: char array array) x y =
    if 0 <= y && y < map.Length && 0 <= x && x < map[y].Length then map[y][x]
    else ' '

let coordinates (twoDArray: 'a array array) =
    [0..twoDArray.Length-1] |> List.collect (fun y -> [0..twoDArray[y].Length-1] |> List.map (fun x -> (x,y)))

type Region = char * (int * int) list

let findRegion (map: char array array) (x: int) (y: int) =
    let regionPlant = getPlant map x y
    let rec _findRegion (x: int) (y: int) (visited: (int * int) list) (region: (int * int) list) =
        if getPlant map x y <> regionPlant || List.contains (x, y) visited then
            (visited, region)
        else
            let westV, westR = _findRegion (x - 1) y ((x, y) :: visited) ((x, y) :: region)
            let northV, northR = _findRegion x (y - 1) westV westR
            let eastV, eastR = _findRegion (x + 1) y northV northR
            let southV, southR = _findRegion x (y + 1) eastV eastR
            (southV, southR)
    (regionPlant, snd (_findRegion x y [] []))

let printRegion (map: char array array) (region: Region) =
    printfn "-----------------------------------"
    for y in 0..map.Length-1 do
        for x in 0..map[y].Length-1 do
            if List.contains (x, y) (snd region) then
                printf $"{map[y][x]}"
            else
                printf "."
        printfn ""


let findRegions (map: char array array) =
    let rec _findRegions (coordinates: (int * int) list) (regions: Region list) =
        match coordinates with
        | (x, y) :: otherCoordinates ->
            let currentRegion = findRegion map x y
            let remainingCoordinates = otherCoordinates |> List.filter (fun pos -> not (List.contains pos (snd currentRegion)))
            _findRegions remainingCoordinates (currentRegion :: regions)
        | [] -> regions
    _findRegions (coordinates map) [] |> List.rev

let allRegions = findRegions input

for region in allRegions do
    printfn $"A region of {fst region} plants with price {snd region |> List.length} * ?? = ???."