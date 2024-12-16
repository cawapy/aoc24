#load "TextInput/Reader.fsx"
open TextInput

let lines = Reader.readInput { Reader.readInputOptions with
                                   EmptyLineTerminator = false
                                   Default = [
                                        //"########";
                                        //"#..O.O.#";
                                        //"##@.O..#";
                                        //"#...O..#";
                                        //"#.#.O..#";
                                        //"#...O..#";
                                        //"#......#";
                                        //"########";
                                        //"";
                                        //"<^^>>>vv<v>>v<<";
                                        "##########";
                                        "#..O..O.O#";
                                        "#......O.#";
                                        "#.OO..O.O#";
                                        "#..O@..O.#";
                                        "#O#..O...#";
                                        "#O..O..O.#";
                                        "#.OO.O.OO#";
                                        "#....O...#";
                                        "##########";
                                        "";
                                        "<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^";
                                        "vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v";
                                        "><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<";
                                        "<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^";
                                        "^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><";
                                        "^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^";
                                        ">^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^";
                                        "<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>";
                                        "^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>";
                                        "v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^";
                                    ]
                               }

type Field = Wall | Box | Space | Robot
type Map = Field list list
type Direction = Up | Down | Left | Right
type Path = Direction list
type Pos = int * int

let parseInput (input: string list) =
    let toMapRow (l: string) =
        l.ToCharArray() |> Array.toList |> List.map (fun x -> match x with | '#' -> Wall | 'O' -> Box | '@' -> Robot | _ -> Space)
    let toDirection (l: string) =
        l.ToCharArray() |> Array.map (fun x -> match x with | '<' -> Left | '>' -> Right | '^' -> Up | _ -> Down) |> Array.toList
    let rec _parse (lines: string list) (mapAcc: string list) (pathAcc: string list) collectPath =
        match lines with
        | "" :: tail -> _parse tail mapAcc pathAcc true
        | l :: tail when collectPath -> _parse tail mapAcc (l::pathAcc) true
        | l :: tail               -> _parse tail (l::mapAcc) pathAcc false
        | [] ->
            let map: Map = mapAcc |> List.rev |> List.map toMapRow
            let path: Path = pathAcc |> List.rev |> List.collect toDirection
            let coordinates: Pos list = [0..map.Length-1] |> List.collect (fun y -> [0..map[y].Length-1] |> List.map (fun x -> (x, y)))
            let startPos: Pos = coordinates |> List.where (fun (x, y) -> map[y][x] = Robot) |> List.item 0
            (map, path, startPos)
    _parse input [] [] false

let printArrayMap (map: Field array array) =
    for row in map do
        for col in row do
            let symbol = match col with | Robot -> "@" | Box -> "O" | Wall -> "#" | _ -> " "
            printf $"{symbol}"
        printfn ""
    printfn ""

let gps (map: Field array array) =
    let coordinates: Pos list = [0..map.Length-1] |> List.collect (fun y -> [0..map[y].Length-1] |> List.map (fun x -> (x, y)))
    coordinates |> List.where (fun (x, y) -> map[y][x] = Box) |> List.sumBy (fun (x, y) -> 100 * y + x)

let runPath (_map: Map) (path: Path) (startPos: Pos) =
    let map: Field array array = _map |> List.map List.toArray |> List.toArray
    let field x y =
        if 0 <= y && y < map.Length && 0 <= x && x < map[y].Length then map[y][x] else Wall
    let step (dx: int) (dy: int) (pos: Pos) =
        let rec _step (dx: int) (dy: int) (pos: Pos) (newField: Field) =
            let x0, y0 = pos
            let x = x0 + dx
            let y = y0 + dy
            match field x y with
            | Space -> map[y][x] <- newField; true
            | Wall -> false
            | Box -> if _step dx dy (x, y) Box then map[y][x] <- newField; true else false
            | Robot -> failwith $"There is an unexpected robot at {x}|{y}"
        if _step dx dy pos Robot then map[snd pos][fst pos] <- Space; (dx + fst pos, dy + snd pos) else pos
    let rec _runPath (path: Path) (pos: Pos) =
        match path with
        | Up :: tail -> _runPath tail (step 0 -1 pos)
        | Down :: tail -> _runPath tail (step 0 1 pos)
        | Left :: tail -> _runPath tail (step -1 0 pos)
        | Right :: tail -> _runPath tail (step 1 0 pos)
        | [] -> []
    printArrayMap map
    _runPath path startPos |> ignore
    printArrayMap map
    gps map

let (map: Map, path: Path, startPos: Pos) = parseInput lines

let totalGps = runPath map path startPos

printfn $"Sum of all boxes' GPS coordinates is {totalGps}"