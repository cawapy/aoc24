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
    let rec step (map: char array array) (guardX: int) (guardY: int) fieldCountAccumulator =
        let fieldValue (x: int) (y: int) =
            if 0 <= y && y < map.Length && 0 <= x && x < map[y].Length then
                map[y][x]
            else ' '
        let guard = map[guardY][guardX]
        let collisionRule = rules |> Seq.where (fun r -> r.Cursor = guard) |> Seq.item 0
        let nextX, nextY = (guardX + fst collisionRule.direction, guardY + snd collisionRule.direction)
        let nextFieldValue = fieldValue nextX nextY
        match nextFieldValue with
        | '#' -> // obstacle ahead: turn and continue without counting a field
                 map[guardY][guardX] <- collisionRule.CursorAfterCollision
                 step map guardX guardY fieldCountAccumulator
        | '.' -> // unvisited field ahead: enter field and continue with counting the field
                 map[guardY][guardX] <- 'X'
                 map[nextY][nextX] <- guard
                 step map nextX nextY fieldCountAccumulator+1
        | 'X' -> // visited field ahead: enter field and continue without counting the field
                 map[guardY][guardX] <- 'X'
                 map[nextY][nextX] <- guard
                 step map nextX nextY fieldCountAccumulator
        | ' ' -> // edge of map ahead: leave behind one more visited field
                 fieldCountAccumulator + 1
        | _ -> failwith $"Unexpected value of next field: '{nextFieldValue}"
    step (input |> List.map _.ToCharArray() |> List.toArray) (fst startPosition) (snd startPosition) 0

let steps = guardSteps input

printfn $"Guard takes {steps} steps"