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

let lines = Reader.readInput { Reader.readInputOptions with Default = sample1 }
let maze = lines |> List.map _.ToCharArray() |> List.toArray

type Maze = char array array
type Pos = int * int
type Path = Pos list

let findPath (maze: Maze) =
    let selectMin (left: ('a list * int) option) (right: ('a list * int) option) =
        match (left, right) with
        | Some (_, l), Some (_, r) when l < r -> left
        | Some (_, l), Some (_, r) when l > r -> right
        | Some (a, l), Some (b, r) when l = r -> Some (a @ b, l)
        | Some _, None -> left
        | None, Some _ -> right
        | _ -> None
    let costArray = maze |> Array.map (fun row -> Array.replicate row.Length 0x7fffffff)
    let visitedArray = maze |> Array.map (fun row -> Array.replicate row.Length false)
    let rec _findPaths (x: int, y: int) (dx: int, dy: int) (path: Path) (cost: int) =
        if cost > costArray[y][x] || visitedArray[y][x] then
            None
        else
            visitedArray[y][x] <- true
            let ret = match maze[y][x] with
                        | '.' | 'S' ->
                            costArray[y][x] <- (cost + 1)
                            let straightOptions = _findPaths (x+dx, y+dy) ( dx,  dy) ((x, y) :: path) (cost+1)
                            costArray[y][x] <- (cost + 1000)
                            let leftOptions     = _findPaths (x+dy, y-dx) ( dy, -dx) ((x, y) :: path) (cost+1001)
                            let rightOptions    = _findPaths (x-dy, y+dx) (-dy,  dx) ((x, y) :: path) (cost+1001)
                            let result = selectMin (selectMin straightOptions leftOptions) rightOptions
                            result
                        | 'E' ->
                            let result: Path = (x, y) :: path |> List.rev
                            Some ([result], cost)
                        | _ -> None
            visitedArray[y][x] <- false
            ret
    let start = [0..maze.Length-1] |> List.collect (fun y -> [0..maze[y].Length-1] |> List.map (fun x -> (x, y))) |>
                List.where (fun (x, y) -> maze[y][x] = 'S') |> List.item 0
    _findPaths start (1, 0) [] 0

let printMazeAndPath (maze: Maze) (paths: Path list) =
    let isInPath (x: int, y: int) (path: Path) =
        path |> List.contains (x, y)
    for y in 0..maze.Length-1 do
        for x in 0..maze[y].Length-1 do
            let tile = maze[y][x]
            let c = match tile with
                        | '.' | 'S' | 'E' ->
                            if List.exists (isInPath (x,y)) paths then '+' else ' '
                        | _ -> tile
            printf $"{c}"
        printfn ""

let sw = System.Diagnostics.Stopwatch.StartNew()
match findPath maze with
| Some (paths, score) ->
    printMazeAndPath maze paths
    printfn $"Lowest score is {score}"
    printfn $"Number of paths is {List.length paths}"
    let total = paths |> List.collect id |> List.distinct |> List.length
    printfn $"Total number of tiles is {total}"
| _ -> printfn "Could not find path"
printfn $"Took {sw.Elapsed}"