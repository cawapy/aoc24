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

let findPaths (maze: Maze) =
    let rec _findPaths (x: int, y: int) (dx: int, dy: int) (path: Path) =
        match maze[y][x] with
        | '.' | 'S' ->
            let pStraight: Path list = if List.contains (x+dx, y+dy) path then [] else _findPaths (x+dx, y+dy) (dx, dy) ((x, y) :: path)
            let pLeft: Path list = if List.contains (x+dy, y-dx) path then [] else _findPaths (x+dy, y-dx) (dy, -dx) ((x, y) :: path)
            let pRight: Path list = if List.contains (x-dy, y+dx) path then [] else _findPaths (x-dy, y+dx) (-dy, dx) ((x, y) :: path)
            pStraight @ pLeft @ pRight
        | 'E' -> printf "P"; [path |> List.rev]
        | _ -> []
    let start = [0..maze.Length-1] |> List.collect (fun y -> [0..maze[y].Length-1] |> List.map (fun x -> (x, y))) |>
                List.where (fun (x, y) -> maze[y][x] = 'S') |> List.item 0
    _findPaths start (1, 0) []

let pathScore (path: Path) =
    let getDirection ((x0, y0): Pos, (x1, y1): Pos) =
        match (x1-x0, y1-y0) with
        | -1,  0 -> 'w'
        | +1,  0 -> 'e'
        |  0, -1 -> 'n'
        |  0, +1 -> 's'
        | _ -> failwith "Unexpected step"
    let directions: char list = path |> List.pairwise |> List.map getDirection |> List.append ['e']
    let turns = directions |> List.pairwise |> List.map (fun (a, b) -> if a <> b then 1 else 0) |> List.sum
    turns * 1000 + path.Length

let paths = findPaths maze
let minScore = paths |> List.map pathScore |> List.min

printfn $"Lowest score is {minScore}"