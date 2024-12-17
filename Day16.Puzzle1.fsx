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
    let getCost (input: ('a * int) option) =
        match input with
        | Some (_, cost) -> Some cost
        | _ -> None
    let minCost (left: int option) (right: int option) =
        match (left, right) with
        | None, None -> None
        | Some _, None -> left
        | None, Some _ -> right
        | Some l, Some r when l <= r -> left
        | _ -> right
    let selectMin (left: ('a * int) option) (right: ('a * int) option) =
        match (left, right) with
        | None, None -> None
        | Some _, None -> left
        | None, Some _ -> right
        | Some (_, l), Some (_, r) when l < r -> left
        | _ -> right
    let rec _findPaths (x: int, y: int) (dx: int, dy: int) (path: Path) (cost: int) (maxCost: int option) =
        match maxCost with
        | Some n when n < cost -> printfn $"cancel: {cost} > max ({n})"; None
        | _ ->
            match maze[y][x] with
            | '.' | 'S' ->
                let straightOption = if List.contains (x+dx, y+dy) path then None else _findPaths (x+dx, y+dy) (dx, dy) ((x, y) :: path) (cost+1) maxCost

                let leftOption = if List.contains (x+dy, y-dx) path then None else _findPaths (x+dy, y-dx) (dy, -dx) ((x, y) :: path) (cost+1001) (getCost straightOption)

                let nonRightOption = selectMin straightOption leftOption
                let rightOption = if List.contains (x-dy, y+dx) path then None else _findPaths (x-dy, y+dx) (-dy, dx) ((x, y) :: path) (cost+1001) (getCost nonRightOption)
                selectMin nonRightOption rightOption
            | 'E' ->
                let result: Path = path |> List.rev
                Some (result, cost)
            | _ -> None
    let start = [0..maze.Length-1] |> List.collect (fun y -> [0..maze[y].Length-1] |> List.map (fun x -> (x, y))) |>
                List.where (fun (x, y) -> maze[y][x] = 'S') |> List.item 0
    _findPaths start (1, 0) [] 0 None

match findPath maze with
| Some (_, score) -> printfn $"Lowest score is {score}"
| _ -> printfn "Could not find path"