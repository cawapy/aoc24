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

let guardSteps (map: string list) =
    let rules = [ { Cursor = '^'; direction = (0,-1); CursorAfterCollision = '>' };
                   { Cursor = '>'; direction = (1,0); CursorAfterCollision = 'v' };
                   { Cursor = 'v'; direction = (0,1); CursorAfterCollision = '<' };
                   { Cursor = '<'; direction = (-1,0); CursorAfterCollision = '^' } ]
    let startPosition =
        let isCursor (pos: int * int) =
            let x, y = pos
            match map[y][x] with | '>' | 'v' | '<' | '^' -> true | _ -> false
        [0 .. map.Length-1]
                        |> Seq.collect (fun y -> [0 .. map[y].Length - 1] |> Seq.map (fun x -> (x, y)))
                        |> Seq.where isCursor
                        |> Seq.item 0
    let rec step (map: char array array) (guardX: int) (guardY: int) (guard: char) (steps: (int * int) list) =
        let fieldValue (x: int) (y: int) =
            if 0 <= y && y < map.Length && 0 <= x && x < map[y].Length then
                map[y][x]
            else ' '
        let collisionRule = rules |> Seq.where (fun r -> r.Cursor = guard) |> Seq.item 0
        let nextX, nextY = (guardX + fst collisionRule.direction, guardY + snd collisionRule.direction)
        let nextFieldValue = fieldValue nextX nextY
        match nextFieldValue with
        // obstacle ahead: turn and continue without doing a true step
        | '#' -> step map guardX guardY collisionRule.CursorAfterCollision steps

        // free field (or guard's starting position) ahead: record step and continue: ...
        | '.' | '^' | '>' | 'v' | '<'
              -> step map nextX nextY guard ((guardX,guardY) :: steps)
        // edge of map ahead: end journey with last step
        | ' ' -> ((guardX,guardY) :: steps)
        | _ -> failwith $"Unexpected value of next field: '{nextFieldValue}"
    let x, y = startPosition
    let mapArray = input |> List.map _.ToCharArray() |> List.toArray
    let allSteps = step mapArray x y mapArray[y].[x] []
    allSteps |> List.distinct |> List.length

let steps = guardSteps input

printfn $"Guard takes {steps} steps"