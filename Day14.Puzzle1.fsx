#load "TextInput/Reader.fsx"
open System
open TextInput

let lines = Reader.readInput { Reader.readInputOptions with
                                   Default = [
                                    "p=0,4 v=3,-3";
                                    "p=6,3 v=-1,-3";
                                    "p=10,3 v=-1,2";
                                    "p=2,0 v=2,-1";
                                    "p=0,0 v=1,3";
                                    "p=3,0 v=-2,-2";
                                    "p=7,6 v=-1,-3";
                                    "p=3,0 v=-1,-2";
                                    "p=9,3 v=2,3";
                                    "p=7,3 v=-1,2";
                                    "p=2,4 v=2,-3";
                                    "p=9,5 v=-3,-3";
                                    ]
                               }

type Pos = int * int
type Velo = int * int
type Robot = Pos * Velo
type Quadrant = TL | TR | BL | BR | None

let parseRobot (line: string) =
    match line.Split('=', ',', ' ') with
    | [| "p"; px; py; "v"; vx; vy |] -> ((Int32.Parse(px), Int32.Parse(py)), (Int32.Parse(vx), Int32.Parse(vy)))
    | _ -> failwith $"Could not parse '{line}'"

let calculatePosition (steps: int) (fieldSize: int*int) (robot: Robot) =
    let (x0, y0), (vx, vy) = robot
    let sx, sy = fieldSize
    (((x0 + steps * vx) % sx + sx) % sx, ((y0 + steps * vy) % sy + sy) % sy)

let getQuadrant (fieldSize: int*int) (pos: Pos) =
    let sx, sy = fieldSize
    let cx, cy = (sx/2, sy/2)
    match pos with
    | x, y when x < cx && y < cy -> TL
    | x, y when x > cx && y < cy -> TR
    | x, y when x < cx && y > cy -> BL
    | x, y when x > cx && y > cy -> BR
    | _ -> None

let safetyFactor (fieldSize: int*int) (positions: Pos list) =
    let robotsInQuadrant (input: Quadrant * Pos list) =
        let _, positions = input
        let result = List.length positions
        result
    let positionsPerQuadrant = positions |> List.groupBy (getQuadrant fieldSize)
    positionsPerQuadrant
        |> List.filter (fun (quadrant, _) -> quadrant <> None)
        |> List.map robotsInQuadrant
        |> List.reduce (fun countA countB -> countA * countB)

let fieldSize = if lines.Length < 20 then (11, 7) else (101, 103)
let steps = 100

let robots = lines |> List.map parseRobot
let allPositions: Pos list = robots |> List.map (calculatePosition steps fieldSize)
let result = safetyFactor fieldSize allPositions

printfn $"Safety factor after 100 seconds will be {result}"