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
    let rec step (map: char array array) (position: int * int) stepsAcc =
        let getField (x: int) (y: int) =
            if 0 <= y && y < map.Length && 0 <= x && x < map[y].Length then
                map[y][x]
            else ' '
        let x, y = position
        let cursor = map[y][x]
        let rule = rules |> Seq.where (fun r -> r.Cursor = cursor) |> Seq.item 0
        let nextX, nextY = (x + fst rule.direction, y + snd rule.direction)
        let nextField = getField nextX nextY
        match nextField with
        | '#' -> map[y][x] <- rule.CursorAfterCollision
                 step map position stepsAcc
        | ' ' -> stepsAcc + 1
        | '.' -> map[y][x] <- 'X'
                 map[nextY][nextX] <- cursor
                 step map (nextX, nextY) stepsAcc+1
        | 'X' -> map[y][x] <- 'X'
                 map[nextY][nextX] <- cursor
                 step map (nextX, nextY) stepsAcc
        | _ -> failwith $"Unexpected value of next field: '{nextField}"
    step (input |> List.map _.ToCharArray() |> List.toArray) startPosition 0

let steps = guardSteps input

printfn $"Guard takes {steps} steps"