#load "TextInput/Reader.fsx"
open TextInput

let input = Reader.readInput { Reader.readInputOptions with
                                   Default = [  "....#.....";
                                                ".........#";
                                                "..........";
                                                "..#.......";
                                                ".......#..";
                                                "..........";
                                                ".#..^.....";
                                                "........#.";
                                                "#.........";
                                                "......#..."; ] }


type Rule = { Cursor: char; direction: int*int; CursorAfterCollision: char }

let findObstacles (stringListMap: string list) =

    let rules = [ { Cursor = '^'; direction = ( 0, -1); CursorAfterCollision = '>' };
                  { Cursor = '>'; direction = ( 1,  0); CursorAfterCollision = 'v' };
                  { Cursor = 'v'; direction = ( 0,  1); CursorAfterCollision = '<' };
                  { Cursor = '<'; direction = (-1,  0); CursorAfterCollision = '^' } ]

    let startPosition (map: char array array) =
        let isCursor (pos: int * int) =
            let x, y = pos
            match map[y][x] with | '>' | 'v' | '<' | '^' -> true | _ -> false
        [0 .. map.Length-1]
                        |> Seq.collect (fun y -> [0 .. map[y].Length - 1] |> Seq.map (fun x -> (x, y)))
                        |> Seq.where isCursor
                        |> Seq.item 0

    let fieldValue (map: char array array) (x: int) (y: int) =
        if 0 <= y && y < map.Length && 0 <= x && x < map[y].Length then
            map[y][x]
        else ' '

    let rec step (map: char array array) (guardX: int) (guardY: int) (guard: char) (steps: (int * int * char) list) =

        if List.contains (guardX, guardY, guard) steps then // already been here, going in same direction -> loop
            []
        else
            let collisionRule = rules |> Seq.where (fun r -> r.Cursor = guard) |> Seq.item 0
            let nextX, nextY = (guardX + fst collisionRule.direction, guardY + snd collisionRule.direction)
            let nextFieldValue = fieldValue map nextX nextY

            match nextFieldValue with

            // obstacle ahead: turn and continue without doing a true step
            | '#' -> step map guardX guardY collisionRule.CursorAfterCollision steps

            // free field (or guard's starting position) ahead: record step and continue: ...
            | '.' | '^' | '>' | 'v' | '<'
                  -> step map nextX nextY guard ((guardX,guardY,guard) :: steps)

            // edge of map ahead: end journey with last step
            | ' ' -> ((guardX,guardY,guard) :: steps)
            | _ -> failwith $"Unexpected value of next field: '{nextFieldValue}"

    let makeArray (input: string list) =
        input |> List.map _.ToCharArray() |> List.toArray

    let originalMap = (makeArray stringListMap)
    let x, y = startPosition originalMap
    let d = fieldValue originalMap x y
    let path = step originalMap x y d [] |> List.rev |> List.distinctBy (fun (x, y, _) -> (x, y))

    // try to place obstacles along path and check again for loop
    let obstacleCausesLoop (startPosition: int * int * char) (obstaclePosition: int * int * _) =
        let x0, y0, d = startPosition
        let xX, yX, _ = obstaclePosition
        if stringListMap[yX][xX] = '.' then
            let modifiedMap = makeArray stringListMap
            modifiedMap[yX][xX] <- '#'
            step modifiedMap x0 y0 d [] = []
        else
            false
    let positions = List.pairwise path |> List.where (fun pair -> obstacleCausesLoop (fst pair) (snd pair))
    positions |> List.length

let numberOfObstacles = findObstacles input

printfn $"Number of possible obstacle positions: {numberOfObstacles}"