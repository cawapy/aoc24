#load "TextInput/Reader.fsx"
open TextInput

let input = Reader.readInput { Reader.readInputOptions with
                                   Default =  [ "............";
                                                "........0...";
                                                ".....0......";
                                                ".......0....";
                                                "....0.......";
                                                "......A.....";
                                                "............";
                                                "............";
                                                "........A...";
                                                ".........A..";
                                                "............";
                                                "............"; ] }

type Antenna = { Frequency: char; X: int; Y: int }
let antennas = [0..input.Length-1] |> List.collect (fun y -> [0..input[y].Length-1] |> List.map (fun x -> { Frequency = input[y][x]; X = x; Y = y })) |> List.where (fun a -> a.Frequency <> '.')
let frequencies = antennas |> List.groupBy _.Frequency

let determineAntiNodes (antennas: Antenna list) =
    let determineAntiNodeOfPair (a: Antenna) (b: Antenna) =
        if a = b then []
        else
            let dx = b.X - a.X
            let dy = b.Y - a.Y
            [ (a.X - dx, a.Y - dy); (b.X + dx, b.Y + dy) ]
    antennas |> List.allPairs antennas |> List.collect (fun (a, b) -> determineAntiNodeOfPair a b)

let allAntiNodes = frequencies |> Seq.collect (fun g -> determineAntiNodes (snd g))|> Seq.distinct

let isOnMap (position: int * int) =
    let x, y = position
    0 <= y && y < input.Length && 0 <= x && x < input[y].Length

let antiNodesOnMap = allAntiNodes |> Seq.where isOnMap |> Seq.length

printfn $"Number of anti nodes is %A{antiNodesOnMap}"