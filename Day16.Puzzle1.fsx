#load "TextInput/Reader.fsx"
open TextInput

let sample1 = [
    "###############";
    "#.......#....E#";
    "#.#.###.#.###.#";
    "#.....#.#...#.#";
    "#.###.#####.#.#";
    "#.#.#.......#.#";
    "#.#.#####.###.#";
    "#...........#.#";
    "###.#.#####.#.#";
    "#...#.....#.#.#";
    "#.#.#.###.#.#.#";
    "#.....#...#.#.#";
    "#.###.#.#.#.#.#";
    "#S..#.....#...#";
    "###############";
]

let sample2 = [
    "#################";
    "#...#...#...#..E#";
    "#.#.#.#.#.#.#.#.#";
    "#.#.#.#...#...#.#";
    "#.#.#.#.###.#.#.#";
    "#...#.#.#.....#.#";
    "#.#.#.#.#.#####.#";
    "#.#...#.#.#.....#";
    "#.#.#####.#.###.#";
    "#.#.#.......#...#";
    "#.#.###.#####.###";
    "#.#.#...#.....#.#";
    "#.#.#.#####.###.#";
    "#.#.#.........#.#";
    "#.#.#.#########.#";
    "#S#.............#";
    "#################";
]

let lines = Reader.readInput { Reader.readInputOptions with Default = sample2 }
let maze = lines |> List.map _.ToCharArray() |> List.toArray

type Maze = char array array
type Pos = int * int
type Path = Pos list

let findPath (maze: Maze) =
    let selectMin (left: int option) (right: int option) =
        match (left, right) with
        | None, None -> None
        | Some _, None -> left
        | None, Some _ -> right
        | Some l, Some r when l < r -> left
        | _ -> right
    let costArray = maze |> Array.map (fun row -> Array.replicate row.Length 0x7fffffff)
    let visitedArray = maze |> Array.map (fun row -> Array.replicate row.Length false)
    let rec _findPaths (x: int, y: int) (dx: int, dy: int) (cost: int) =
        if costArray[y][x] < cost || visitedArray[y][x] then None
        else
            costArray[y][x] <- cost
            visitedArray[y][x] <- true
            let ret = match maze[y][x] with
                        | '.' | 'S' ->
                            let straightOption = _findPaths (x+dx, y+dy) (dx, dy) (cost+1)
                            let leftOption = _findPaths (x+dy, y-dx) (dy, -dx) (cost+1001)
                            let rightOption = _findPaths (x-dy, y+dx) (-dy, dx) (cost+1001)
                            selectMin (selectMin straightOption leftOption) rightOption
                        | 'E' ->
                            Some cost
                        | _ -> None
            visitedArray[y][x] <- false
            ret
    let start = [0..maze.Length-1] |> List.collect (fun y -> [0..maze[y].Length-1] |> List.map (fun x -> (x, y))) |>
                List.where (fun (x, y) -> maze[y][x] = 'S') |> List.item 0
    _findPaths start (1, 0) 0

let sw = System.Diagnostics.Stopwatch.StartNew()
match findPath maze with
| Some score -> printfn $"Lowest score is {score}"
| _ -> printfn "Could not find path"
printfn $"Took {sw.Elapsed}"