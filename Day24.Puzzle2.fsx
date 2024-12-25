open System

type Operation = AND | OR | XOR
type WireName = string
type Gate = WireName * Operation * WireName
type WireDefinition = Function of Gate | Evaluated of bool
type Definitions = Map<WireName, WireDefinition>

let parseInput (lines: string list) : (WireName * WireDefinition) list =
    let parseLine (line: string) : (WireName * WireDefinition) option =
        match line.Split([|':';' '|], StringSplitOptions.RemoveEmptyEntries) with
        | [| name; "0" |] -> Some (name, Evaluated false)
        | [| name; "1" |] -> Some (name, Evaluated true)
        | [| in1; "AND"; in2; "->"; name |] -> Some (name, Function (in1, AND, in2))
        | [| in1; "XOR"; in2; "->"; name |] -> Some (name, Function (in1, XOR, in2))
        | [| in1; "OR"; in2; "->"; name |] -> Some (name, Function (in1, OR, in2))
        | _ -> None
    lines |> List.map parseLine |> List.choose id

let wireName (prefix: string) (bit: int) : string =
    sprintf $"{prefix}%02d{bit}"

let solvePuzzle (lines: string list) : unit =
    let definitions = parseInput lines |> Map.ofList
    let findWire (a: WireName) (op: Operation) (b: WireName) =
       definitions |> Map.findKey (fun _ definition ->
           match definition with
           | Function (inputA, operation, inputB) when inputA = a && inputB = b && operation = op -> true
           | Function (inputB, operation, inputA) when inputA = a && inputB = b && operation = op -> true
           | _ -> false
       )
    let tryFindWire2 (a: WireName) (op: Operation) (b: WireName) =
       definitions |> Map.tryFindKey (fun _ definition ->
           match definition with
           | Function (inputA, operation, inputB) when inputA = a && inputB = b && operation = op -> true
           | Function (inputB, operation, inputA) when inputA = a && inputB = b && operation = op -> true
           | _ -> false
       )
    let tryFindWire1 (a: WireName) (op: Operation) : (WireName * WireName) option =
       definitions |> Map.tryPick (fun k definition ->
           match definition with
           | Function (inputA, operation, b) when inputA = a && operation = op -> Some (k, b)
           | Function (b, operation, inputA) when inputA = a && operation = op -> Some (k, b)
           | _ -> None
       )

    let rec checkAddStage (bit: int) (carry: WireName) (wrongWires: WireName list) : WireName list =
        match tryFindWire2 (wireName "x" bit) XOR (wireName "y" bit) with
        | None -> wrongWires
        | Some inputXor ->
            match tryFindWire2 carry AND inputXor with
            | Some internalAnd -> checkAddStage' bit inputXor carry internalAnd wrongWires
            | None ->
                match tryFindWire1 carry AND with
                | Some (internalAnd, actualInputXor) ->
                        checkAddStage' bit actualInputXor carry internalAnd (inputXor::actualInputXor::wrongWires)
                | None ->
                    match tryFindWire1 inputXor AND with
                    | Some (internalAnd, actualCarry) ->
                        checkAddStage' bit inputXor actualCarry internalAnd (actualCarry::carry::wrongWires)
                    | None -> failwith $"Could not find anything related to internal And gate at bit {bit}"

    and checkAddStage' (bit: int) (inputXor: WireName) (carry: WireName) (internalAnd: WireName) (wrongWires: WireName list) : WireName list =
        match tryFindWire2 inputXor XOR carry with
        | Some outputXor -> checkAddStage'' bit outputXor internalAnd wrongWires
        | None ->
            match tryFindWire1 inputXor XOR with
            | Some (outputXor, actualInputXor) ->
                checkAddStage'' bit outputXor internalAnd (inputXor::actualInputXor::wrongWires)
            | None ->
                match tryFindWire1 carry XOR with
                | Some (outputXor, actualCarry) ->
                    checkAddStage'' bit outputXor internalAnd (carry::actualCarry::wrongWires)
                | None -> failwith $"Could not find anything related to output Xor gate at bit {bit}"

    and checkAddStage'' (bit: int) (outputXor: WireName) (internalAnd: WireName) (wrongWires: WireName list) : WireName list =
        let inputAnd = findWire (wireName "x" bit) AND (wireName "y" bit)

        match tryFindWire2 internalAnd OR inputAnd with
        | Some outputOr -> checkAddStage''' bit outputXor outputOr wrongWires
        | None ->
            match tryFindWire1 inputAnd OR with
            | Some (outputOr, actualInternalAnd) ->
                checkAddStage''' bit outputXor outputOr (internalAnd::actualInternalAnd::wrongWires)
            | None ->
                match tryFindWire1 internalAnd OR with
                | Some (outputOr, actualInputAnd) ->
                    checkAddStage''' bit outputXor outputOr (inputAnd::actualInputAnd::wrongWires)
                | None -> failwith $"Could not find anything related to output Or gate at bit {bit}"

    and checkAddStage''' (bit: int) (outputXor: WireName) (outputOr: WireName) (wrongWires: WireName list) : WireName list =
        let zn = wireName "z" bit
        let wrongWires' = if outputXor <> zn then (outputXor::zn::wrongWires)
                          else wrongWires
        checkAddStage (bit+1) outputOr wrongWires'

    let checkAddStage0 () : WireName list =
        let inputXor = findWire (wireName "x" 0) XOR (wireName "y" 0)
        let carry = findWire (wireName "x" 0) AND (wireName "y" 0)
        let wrongWires = if inputXor = (wireName "z" 0) then [] else [(wireName "z" 0); inputXor]
        checkAddStage 1 carry wrongWires

    let wrong = checkAddStage0 ()
    let result = String.Join(",", wrong |> List.distinct |> List.sort)
    printfn $"Wrong wires: {result}"
    ()

#load "TextInput/Reader.fsx"
let lines = TextInput.Reader.readInput { TextInput.Reader.readInputOptions with EmptyLineTerminator = false }
solvePuzzle lines