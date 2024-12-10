#load "TextInput/Reader.fsx"
open TextInput

let lines = Reader.readInput { Reader.readInputOptions with
                                   Default =  [
                                       "89010123";
                                       "78121874";
                                       "87430965";
                                       "96549874";
                                       "45678903";
                                       "32019012";
                                       "01329801";
                                       "10456732";
                                   ] }

let input = lines |> List.map (fun l -> l.ToCharArray() |> Array.map (fun c -> int (c - '0'))) |> List.toArray

let rec getZ (map: int array array) (x: int) (y: int) =
    if 0 <= y && y < map.Length && 0 <= x && x < map[y].Length then map[y][x]
    else -1

let getTrailRating (map: int array array) (x: int) (y: int) =
    let rec findTrails (x: int) (y: int) (z: int) =
        if getZ map x y = z then
            if z = 9 then [(x,y)]
            else
                findTrails (x - 1) y (z + 1) @
                findTrails (x + 1) y (z + 1) @
                findTrails x (y - 1) (z + 1) @
                findTrails x (y + 1) (z + 1)
        else []
    findTrails x y 0 |> List.length

let allCoordinates (twoD: 'a array array) =
    [0..twoD.Length-1] |> Seq.collect (fun y -> [0..twoD[y].Length-1] |> Seq.map (fun x -> (x, y)))

let ratings = allCoordinates input |> Seq.map (fun (x, y) -> getTrailRating input x y) |> Seq.toList
let sum = ratings |> List.sum
printf $"Sum of ratings is {sum}"