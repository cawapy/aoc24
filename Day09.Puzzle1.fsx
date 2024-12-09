
printfn "Enter input and terminate with ^Z aka EOL"
let fromStdIn = System.Console.In.ReadToEnd().Trim()
let input = if fromStdIn <> "" then fromStdIn else "2333133121414131402"

type Block = File of int | None

let toBlockMap (diskMap: string) =
    let endOfDisk = diskMap.Length
    let rec _readBlockMap (pos: int) (acc: Block list) =
        if pos = endOfDisk then acc
        else
            let size = int (diskMap[pos] - '0')
            let block = if pos % 2 = 0 then File (pos / 2) else None
            _readBlockMap (pos + 1) (acc @ List.replicate size block)
    _readBlockMap 0 []

let format (blockMap: Block list) =
    let elements = blockMap |> List.map (fun b -> match b with | File f -> $"{f}" | _ -> ".")
    String.concat "" elements

let compactBlocks (blockMap: Block list) =
    let rec _compactBlocks (front: Block list) (back: Block list) (blocksToProcess: int) acc =
        if blocksToProcess = 0 then acc |> List.rev
        else
            match front with
            | File _ as fileBlock :: frontTail ->
                _compactBlocks frontTail back (blocksToProcess - 1) (fileBlock :: acc)
            | None :: frontTail ->
                match back with
                | File _ as fileBlock :: backTail ->
                    _compactBlocks frontTail backTail (blocksToProcess - 2) (fileBlock :: acc)
                | None :: backTail ->
                    _compactBlocks (None :: frontTail) backTail (blocksToProcess - 1) acc
                | [] ->
                    failwith "unexpected end of back"
            | [] ->
                failwith "unexpected end of front"
    _compactBlocks blockMap (List.rev blockMap) (List.length blockMap) []

let inputBm = toBlockMap input
let outputBm = compactBlocks inputBm

let checksum = outputBm |> Seq.mapi (fun position value -> int64 (position * match value with File f -> f | _ -> 0)) |> Seq.sum

printfn $"Checksum is {checksum}"