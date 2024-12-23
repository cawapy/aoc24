#load "TextInput/Reader.fsx"
open System.Collections.Generic
open TextInput
let sample = [
   "029A"
   "980A"
   "179A"
   "456A"
   "379A"
]
let lines = Reader.readInput { Reader.readInputOptions with Default = sample }

type Pos = { x: int; y: int }
let pos x y = { x = x; y = y }
let subtractPos (a: Pos) (b: Pos) : Pos = { x = a.x - b.x; y = a.y - b.y }

type NumKey = Num of int | NpA
type ArrowKey = L | R | U | D | A

let numKeypadPositions : Map<NumKey,Pos> =
  [
    (Num 7, pos 0 0); (Num 8, pos 1 0); (Num 9, pos 2 0);
    (Num 4, pos 0 1); (Num 5, pos 1 1); (Num 6, pos 2 1);
    (Num 1, pos 0 2); (Num 2, pos 1 2); (Num 3, pos 2 2);
                      (Num 0, pos 1 3); (NpA  , pos 2 3)
  ] |> Map.ofList

let arrowKeypadPositions : Map<ArrowKey,Pos> =
  [
                  (U, pos 1 0); (A, pos 2 0);
    (L, pos 0 1); (D, pos 1 1); (R, pos 2 1);
  ] |> Map.ofList

let typeRemoteKeys (remoteKeys: 'key list) (findPos: 'key -> Pos) (startPos: Pos) (badPos: Pos) : ArrowKey list =
    let typeKey (key: 'key) (currentPos: Pos) : ArrowKey list * Pos =
        let newPos = findPos key
        let delta = subtractPos newPos currentPos
        let xSteps = List.replicate (abs delta.x) (if delta.x < 0 then L else R)
        let ySteps = List.replicate (abs delta.y) (if delta.y < 0 then U else D)
        let steps = if currentPos.x = badPos.x && newPos.y = badPos.y then // avoid empty key position
                        xSteps @ ySteps
                    elif currentPos.y = badPos.y && newPos.x = badPos.x then // avoid empty key position
                        ySteps @ xSteps
                    elif delta.x < 0 then  xSteps @ ySteps // prefer <
                    elif delta.y <> 0 then ySteps @ xSteps // ...then ^ and v
                    else                   xSteps @ ySteps // ...then >
        (steps @ [A], newPos)
    let rec _typeKeys (remoteKeys: 'key list) (remotePos: Pos) (allLocalKeys: ArrowKey list list) : ArrowKey list =
        match remoteKeys with
        | remoteKey :: remoteTail ->
            let localKeys, newRemotePosition = typeKey remoteKey remotePos
            _typeKeys remoteTail newRemotePosition (localKeys :: allLocalKeys)
        | [] -> allLocalKeys |> List.rev |> List.concat
    _typeKeys remoteKeys startPos []

let typeNumKeys (remoteKeys: NumKey list) : ArrowKey list =
    let getNumKeyPos (k: NumKey) : Pos =
        numKeypadPositions |> Map.find k
    typeRemoteKeys remoteKeys getNumKeyPos (getNumKeyPos NpA) (pos 0 3)

let typeArrowKeys (remoteKeys: ArrowKey list) : ArrowKey list =
    let getArrowKeyPos (k: ArrowKey) : Pos =
        arrowKeypadPositions |> Map.find k
    typeRemoteKeys remoteKeys getArrowKeyPos (getArrowKeyPos A) (pos 0 0)

let formatNumKeys(keys: NumKey list) : string =
    let formatNumKey(numKey: NumKey) : string =
        match numKey with | Num i -> $"{i}" | NpA -> "A"
    String.concat "" (keys |> List.map formatNumKey)

let formatArrowKeys(keys: ArrowKey list) : string =
    let formatArrowKey(arrowKey: ArrowKey) : string =
        match arrowKey with | U -> "^" | D -> "v" | L -> "<" | R -> ">" | A -> "A"
    String.concat "" (keys |> List.map formatArrowKey)

let typeKeysIndirectly (remoteNumKeys: NumKey list) (levels: int): int64 =
    let cache = Dictionary<int * ArrowKey list, int64>()
    let rec splitAtA (acc0: ArrowKey list) (acc: ArrowKey list list) (input: ArrowKey list) : ArrowKey list list =
        match input with
        | [] -> acc |> List.rev
        | A :: tail -> splitAtA [] (((A::acc0) |> List.rev) :: acc) tail
        | x :: tail -> splitAtA (x::acc0) acc tail
    let rec typeIndirectly (remoteArrowKeys: ArrowKey list) levels : int64=
        let getPartLength (part: ArrowKey list) : int64 =
            match cache.TryGetValue((levels, part)) with
            | true, result -> result
            | false, _ ->
                let result = typeIndirectly (typeArrowKeys part) (levels - 1)
                cache.Add((levels, part), result)
                result
        if levels = 0 then remoteArrowKeys |> List.length |> int64
        else remoteArrowKeys |> splitAtA [] [] |> List.map getPartLength |> List.sum
    typeIndirectly (typeNumKeys remoteNumKeys) levels

let calculateComplexity (levels: int) (numKeys: NumKey list) =
    let rec getNumber (numKeys: NumKey list) valueAcc =
        match numKeys with
        | [] -> valueAcc
        | NpA :: _ -> valueAcc
        | Num n :: tail -> getNumber tail (valueAcc * 10 + n)
    let numberOfStrokes = typeKeysIndirectly numKeys levels
    let enteredNumber = getNumber numKeys 0
    let complexity = numberOfStrokes * int64 enteredNumber
    complexity

let parseLines (lines: string list) : NumKey list list =
    let parseLine (line: string) : NumKey list =
        line.ToCharArray() |> Seq.map (fun c ->
            if '0' <= c && c <= '9' then Num (int (c - '0'))
            elif c = 'A' then NpA
            else failwith $"Unknown key '{c}'" ) |> Seq.toList
    lines |> List.map parseLine

let input = parseLines lines
let totalPart1 = input |> List.sumBy (calculateComplexity 2)
let totalPart2 = input |> List.sumBy (calculateComplexity 25)
printfn $"The sum of complexities for part 1 is {totalPart1}"
printfn $"The sum of complexities for part 2 is {totalPart2}"