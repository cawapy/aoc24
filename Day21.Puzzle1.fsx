#load "TextInput/Reader.fsx"
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
let pos x y =
    { x = x; y = y }

type NumKey = Num of int | NpA
type ArrowKey = L | R | U | D | A

let numKeypadPositions : Map<NumKey,Pos> =
  [
    Num 7, pos 0 0; Num 8, pos 1 0; Num 9, pos 2 0;
    Num 4, pos 0 1; Num 5, pos 1 1; Num 6, pos 2 1;
    Num 1, pos 0 2; Num 2, pos 1 2; Num 3, pos 2 2;
                    Num 0, pos 1 3; NpA  , pos 2 3
  ] |> Map.ofList

let arrowKeypadPositions : Map<ArrowKey,Pos> =
  [
                U, pos 1 0; A, pos 2 0;
    L, pos 0 1; D, pos 1 1; R, pos 2 1;
  ] |> Map.ofList

let typeNumKeys (remoteKeys: NumKey list) : ArrowKey list =
    let typeNumKey (key: NumKey) (currentPosition: Pos) : ArrowKey list * Pos =
        let newPosition : Pos = numKeypadPositions |> Map.find key
        let xSteps = newPosition.x - currentPosition.x
        let ySteps = newPosition.y - currentPosition.y
        let moveSteps = List.replicate (abs xSteps) (if xSteps < 0 then L else R) @
                        List.replicate (abs ySteps) (if ySteps < 0 then U else D)
        (moveSteps @ [A], newPosition)
    let rec _typeNumKeys (remoteKeys: NumKey list) (remotePosition: Pos) (allLocalKeys: ArrowKey list list) : ArrowKey list =
        match remoteKeys with
        | remoteKey :: remoteTail ->
            let localKeys, newRemotePosition = typeNumKey remoteKey remotePosition
            _typeNumKeys remoteTail newRemotePosition (localKeys :: allLocalKeys)
        | [] -> allLocalKeys |> List.rev |> List.concat
    let initialRemotePosition = numKeypadPositions |> Map.find NpA
    _typeNumKeys remoteKeys initialRemotePosition []

let typeArrowKeys (remoteKeys: ArrowKey list) : ArrowKey list =
    let typeArrowKey (key: ArrowKey) (currentPos: Pos) : ArrowKey list * Pos =
        let newPosition : Pos = arrowKeypadPositions |> Map.find key
        let xSteps = newPosition.x - currentPos.x
        let ySteps = newPosition.y - currentPos.y
        let moveSteps = List.replicate (abs xSteps) (if xSteps < 0 then L else R) @
                        List.replicate (abs ySteps) (if ySteps < 0 then U else D)
        (moveSteps @ [A], newPosition)
    let rec _typeArrowKey (remoteKeys: ArrowKey list) (remotePosition: Pos) (allLocalKeys: ArrowKey list list) : ArrowKey list =
        match remoteKeys with
        | remoteKey :: remoteKeyTail ->
            let localKeys, newRemotePosition = typeArrowKey remoteKey remotePosition
            _typeArrowKey remoteKeyTail newRemotePosition (localKeys :: allLocalKeys)
        | [] -> allLocalKeys |> List.rev |> List.concat
    let initialRemotePosition = arrowKeypadPositions |> Map.find A
    _typeArrowKey remoteKeys initialRemotePosition []

let formatNumKeys(keys: NumKey list) : string =
    let formatNumKey(numKey: NumKey) : string =
        match numKey with
        | Num i -> $"{i}"
        | NpA -> "A"
    String.concat "" (keys |> List.map formatNumKey)


let formatArrowKeys(keys: ArrowKey list) : string =
    let formatArrowKey(arrowKey: ArrowKey) : string =
        match arrowKey with
        | U -> "^" | D -> "v"
        | L -> "<" | R -> ">"
        | A -> "A"
    String.concat "" (keys |> List.map formatArrowKey)

let typeKeysIndirectly (remoteNumKeys: NumKey list) =
    let rec typeIndirectly (remoteArrowKeys: ArrowKey list) levels =
        if levels = 0 then remoteArrowKeys
        else typeIndirectly (typeArrowKeys remoteArrowKeys) (levels - 1)
    typeIndirectly (typeNumKeys remoteNumKeys) 2

let calculateComplexity (numKeys: NumKey list) =
    let rec getNumber (numKeys: NumKey list) valueAcc =
        match numKeys with
        | [] -> valueAcc
        | NpA :: _ -> valueAcc
        | Num n :: tail -> getNumber tail (valueAcc * 10 + n)
    let arrowKeys = typeKeysIndirectly numKeys
    let numberOfStrokes = arrowKeys |> List.length
    let enteredNumber = getNumber numKeys 0
    let complexity = numberOfStrokes * enteredNumber
    printfn $"{formatNumKeys numKeys}: %-70s{formatArrowKeys arrowKeys} // {numberOfStrokes} * %3d{enteredNumber} = %5d{complexity}"
    complexity



let parseLines (lines: string list) : NumKey list list =
    let parseLine (line: string) : NumKey list =
        line.ToCharArray() |> Seq.map (fun c ->
            if '0' <= c && c <= '9' then Num (int (c - '0'))
            elif c = 'A' then NpA
            else failwith $"Unknown key '{c}'" ) |> Seq.toList
    lines |> List.map parseLine

let input = parseLines lines
let total = input |> List.sumBy calculateComplexity
printfn $"The sum of complexities is {total}"
// not: 241370