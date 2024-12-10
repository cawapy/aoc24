
printfn "Enter input and terminate with ^Z aka EOL"
let fromStdIn = System.Console.In.ReadToEnd().Trim()
let input = if fromStdIn <> "" then fromStdIn else "2333133121414131402"

type Block = File of int | Unassigned

let toBlockMap (diskMap: string) =
    let endOfDisk = diskMap.Length
    let rec prepend (block: Block) (count: int) (acc: Block list) =
        if count = 0 then acc
        else prepend block (count - 1) (block :: acc)
    let rec _readBlockMap (pos: int) (acc: Block list) =
        if pos = endOfDisk then acc |> List.rev
        else
            let size = int (diskMap[pos] - '0')
            let block = if pos % 2 = 0 then File (pos / 2) else Unassigned
            _readBlockMap (pos + 1) (prepend block size acc)
    _readBlockMap 0 []

let format (blockMap: Block list) =
    let elements = blockMap |> List.map (fun b -> match b with | File f -> $"{f}" | _ -> ".")
    String.concat "" elements

let compactBlocks (blockMap: Block list) =
    let rec extractFile (id: int) (input: Block list) (lengthAcc: int) (replaced: Block list) (unreplaced: Block list) =
        match input with
        | File i as f :: tail when i = id -> extractFile id tail (lengthAcc + 1) (Unassigned :: replaced) (f :: unreplaced)
        | f :: tail -> (lengthAcc, f::tail, replaced, unreplaced)
        | []        -> (lengthAcc, [],   replaced, unreplaced)
    let rec tryInsert (input: Block list) (length: int) (id: int) (lengthAcc: int) (inserted: Block list) (notInserted: Block list) =
        match input with
        | Unassigned :: tail when lengthAcc + 1 = length
            -> Some ((List.rev (File id :: inserted)) @ tail)
        | Unassigned :: tail
            -> tryInsert tail length id (lengthAcc+1) (File id :: inserted) (Unassigned :: notInserted)
        | File x :: _ when x = id // own file detected; don't move
            -> None
        | File _ as f :: tail
            -> tryInsert tail length id 0 (f :: notInserted) (f :: notInserted)
        | []
            -> None
    let rec _compactBlocks (front: Block list) (back: Block list) (backAcc: Block list) =
            match back with
            | File id as f :: backTail ->
                let length, remainingBackTail, extractedAcc, unextractedAcc = extractFile id backTail 1 (Unassigned::backTail) (f::backTail)
                let inserted = tryInsert front length id 0 [] []
                match inserted with
                | Some blocks ->
                    printfn $"Moved {id} ({length}x): {format blocks}"
                    _compactBlocks blocks remainingBackTail extractedAcc
                | None ->
                    printfn $"Kept  {id} ({length}x): {format front}"
                    _compactBlocks front remainingBackTail unextractedAcc
            | Unassigned :: backTail ->
                        _compactBlocks front backTail (Unassigned::backAcc)
            | [] -> backAcc |> List.rev
    _compactBlocks blockMap (List.rev blockMap) []

let inputBm = toBlockMap input
printfn $"{format inputBm}"
let outputBm = compactBlocks inputBm
printfn $"{format outputBm}"

let checksum = outputBm |> Seq.mapi (fun position value -> int64 (position * match value with File f -> f | _ -> 0)) |> Seq.sum

printfn $"Checksum is {checksum}"