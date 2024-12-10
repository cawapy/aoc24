
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

    let blocks = blockMap |> List.toArray

    let rec findEndOfGap length (startIndex: int) (index: int) =
        if index >= blocks.Length then None
        elif index - startIndex = length then Some startIndex
        else match blocks[index] with
                | Unassigned -> findEndOfGap length startIndex (index + 1)
                | _          -> findEndOfGap length (index + 1) (index + 1)

    let rec findGap length i =
        if i >= blocks.Length then None
        else match blocks[i] with
                | Unassigned ->
                        match findEndOfGap length i i with
                        | Some start -> Some start
                        | None -> findGap length (i+1)
                | _ -> findGap length (i+1)

    let rec findStartOfFile (f: int) (endIndex: int) (index: int) =
        if index = 0 then Some (f, 0, endIndex)
        else match blocks[index-1] with
                | File g when g = f -> findStartOfFile f endIndex (index - 1)
                | _                 -> Some (f, index, endIndex - index)

    let rec findFile i =
        if i = 0 then None
        else match blocks[i-1] with
                | File f -> findStartOfFile f i (i - 1)
                | _ -> findFile (i - 1)

    let rec relocate fromIndex toIndex count =
        if count = 0 then ()
        else
            blocks[toIndex] <- blocks[fromIndex]
            blocks[fromIndex] <- Unassigned
            relocate (fromIndex+1) (toIndex+1) (count-1)

    let tryRelocate fileLength fileStart =
        match findGap fileLength 0 with
        | Some gapStart when gapStart < fileStart
            -> relocate fileStart gapStart fileLength
        | _ -> ()

    let rec _compactBlocks (index: int) =

        if index <= 0 then blocks |> Array.toList
        else match findFile index with
                | None ->
                    _compactBlocks (index - 1)
                | Some (file, start, len) ->
                    tryRelocate len start
                    _compactBlocks start
    _compactBlocks blocks.Length

let inputBm = toBlockMap input
let outputBm = compactBlocks inputBm

let checksum = outputBm |> Seq.mapi (fun position value -> int64 (position * match value with File f -> f | _ -> 0)) |> Seq.sum

printfn $"Checksum is {checksum}"