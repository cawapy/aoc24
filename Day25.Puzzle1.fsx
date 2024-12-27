
type Key = Key of int list
type Lock = Lock of int list

let solvePuzzle (locks: Lock list) (keys: Key list) : unit =
    let lockAndKeyMatch (Lock l: Lock, Key k: Key) : bool =
        l |> List.zip k |> List.forall (fun (x, y) -> x + y <= 5)
    let matches = keys |> Seq.allPairs locks |> Seq.where lockAndKeyMatch |> Seq.length
    printfn $"There are {matches} matching lock/key pairs"
    ()

type KL = K of Key | L of Lock
let parse (lines: string list) : Lock list * Key list =
    let addLine (il: int list) (s: string) : int list =
        s.ToCharArray() |> Seq.map (fun c -> match c with | '.' -> 0 | '#' -> 1 | _ -> failwith "unexpected symbol" )
            |> Seq.zip il |> Seq.map (fun (x, y) -> x+y) |> Seq.toList
    let rec _parse (n: int) (lines: string list) (current: KL option) (locks: Lock list) (keys: Key list) (ln: int) =
        match (lines, current) with
        | [], _ -> (locks, keys)
        | "" :: "#####" :: tail, None                         -> _parse 0     tail (Some (K (Key  [0;0;0;0;0])))     locks      keys  (ln+2)
        | "" :: "....." :: tail, None                         -> _parse 0     tail (Some (L (Lock [0;0;0;0;0])))     locks      keys  (ln+2)
        | "....."       :: tail, Some (K k)        when n = 5 -> _parse 0     tail  None                             locks  (k::keys) (ln+1)
        | "#####"       :: tail, Some (L l)        when n = 5 -> _parse 0     tail  None                         (l::locks)     keys  (ln+1)
        | s             :: tail, Some (K (Key  k)) when n < 5 -> _parse (n+1) tail (Some (K (Key  (addLine k s))))   locks      keys  (ln+1)
        | s             :: tail, Some (L (Lock l)) when n < 5 -> _parse (n+1) tail (Some (L (Lock (addLine l s))))   locks      keys  (ln+1)
        | _ -> failwith $"Unexpected state in line {ln}"
    _parse 0 ("" :: lines) None [] [] 0


let sample = [
    "#####"; ".####"; ".####"; ".####"; ".#.#."; ".#..."; "....."; "";
    "#####"; "##.##"; ".#.##"; "...##"; "...#."; "...#."; "....."; "";
    "....."; "#...."; "#...."; "#...#"; "#.#.#"; "#.###"; "#####"; "";
    "....."; "....."; "#.#.."; "###.."; "###.#"; "###.#"; "#####"; "";
    "....."; "....."; "....."; "#...."; "#.#.."; "#.#.#"; "#####";
]

#load "TextInput/Reader.fsx"
let lines = TextInput.Reader.readInput { TextInput.Reader.readInputOptions with EmptyLineTerminator = false; Default = sample }
let locks, keys = parse lines
solvePuzzle locks keys