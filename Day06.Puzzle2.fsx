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

    // array preferred over list for frequent random access
    let run (map: char array array) =

        let rules = [ { Cursor = '^'; direction = ( 0, -1); CursorAfterCollision = '>' };
                      { Cursor = '>'; direction = ( 1,  0); CursorAfterCollision = 'v' };
                      { Cursor = 'v'; direction = ( 0,  1); CursorAfterCollision = '<' };
                      { Cursor = '<'; direction = (-1,  0); CursorAfterCollision = '^' } ]

        let fieldValue (x: int) (y: int) =
            if 0 <= y && y < map.Length && 0 <= x && x < map[y].Length then
                map[y][x]
            else ' '

        let startPosition =
            let isCursor (pos: int * int) =
                let x, y = pos
                match map[y][x] with | '>' | 'v' | '<' | '^' -> true | _ -> false
            [0 .. map.Length-1]
                            |> Seq.collect (fun y -> [0 .. map[y].Length - 1] |> Seq.map (fun x -> (x, y)))
                            |> Seq.where isCursor
                            |> Seq.item 0

        let rec step (guardX: int) (guardY: int) (guard: char) (steps: (int * int * char) list) =

            if List.contains (guardX, guardY, guard) steps then // already been here, going in same direction -> loop
                []
            else
                let collisionRule = rules |> Seq.where (fun r -> r.Cursor = guard) |> Seq.item 0
                let nextX, nextY = (guardX + fst collisionRule.direction, guardY + snd collisionRule.direction)
                let nextFieldValue = fieldValue nextX nextY

                match nextFieldValue with

                // obstacle ahead: turn and continue without doing a true step
                | '#' -> step guardX guardY collisionRule.CursorAfterCollision steps

                // free field (or guard's starting position) ahead: record step and continue: ...
                | '.' | '^' | '>' | 'v' | '<'
                      -> step nextX nextY guard ((guardX,guardY,guard) :: steps)

                // edge of map ahead: end journey with last step
                | ' ' -> ((guardX,guardY,guard) :: steps)
                | _ -> failwith $"Unexpected value of next field: '{nextFieldValue}"

        let x, y = startPosition
        step x y map[y].[x] []

    let makeArray (input: string list) =
        input |> List.map _.ToCharArray() |> List.toArray

    let path = run (makeArray stringListMap) |> List.map (fun (x, y, _) -> (x, y)) |> List.distinct

    // try to place obstacles along path and check again for loop
    let obstacleCausesLoop (position: int * int) =
        let x, y = position
        let modifiedMap = makeArray stringListMap
        if modifiedMap[y][x] = '^' then
            false
        else
            modifiedMap[y][x] <- '#'
            run modifiedMap = []
    let positions = path |> List.where obstacleCausesLoop
    positions |> List.length

let numberOfObstacles = findObstacles input

printfn $"Number of possible obstacle positions: {numberOfObstacles}"