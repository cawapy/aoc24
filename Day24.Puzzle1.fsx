open System

type Operation = AND | OR | XOR
type WireName = string
type Gate = WireName * Operation * WireName
type WireDefinition = Function of Gate | Evaluated of bool

let rec solveWire (definitions: Map<WireName, WireDefinition>) (name: WireName) : bool =
    let evaluate (op: Operation) (a: bool) (b: bool) : bool =
        match op with
        | AND -> a && b
        | OR -> a || b
        | XOR -> a <> b
    let definition = definitions[name]
    match definition with
    | Evaluated e -> e
    | Function (w1, op, w2) -> evaluate op (solveWire definitions w1) (solveWire definitions w2)

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

let solvePuzzle (lines: string list) : unit =
    let definitions = parseInput lines |> Map.ofList
    let zNames = definitions.Keys |> Seq.where (fun (s : string) -> s.StartsWith("z")) |> Seq.toList
    let rec _solveWires (names: WireName list) (acc: int64) : int64 =
        match names with
        | [] -> acc
        | wire :: tail ->
            let bitResult = solveWire definitions wire
            let newAcc = if bitResult then
                            let position = Int32.Parse (wire.Substring(1))
                            acc + (1L <<< position)
                         else acc
            _solveWires tail newAcc
    let result = _solveWires zNames 0
    printfn $"Result is {result}"

let sample1 = [
    "x00: 1";
    "x01: 1";
    "x02: 1";
    "y00: 0";
    "y01: 1";
    "y02: 0";
    "";
    "x00 AND y00 -> z00";
    "x01 XOR y01 -> z01";
    "x02 OR y02 -> z02";
]
let sample2 = [
    "x00: 1";
    "x01: 0";
    "x02: 1";
    "x03: 1";
    "x04: 0";
    "y00: 1";
    "y01: 1";
    "y02: 1";
    "y03: 1";
    "y04: 1";
    "";
    "ntg XOR fgs -> mjb";
    "y02 OR x01 -> tnw";
    "kwq OR kpj -> z05";
    "x00 OR x03 -> fst";
    "tgd XOR rvg -> z01";
    "vdt OR tnw -> bfw";
    "bfw AND frj -> z10";
    "ffh OR nrd -> bqk";
    "y00 AND y03 -> djm";
    "y03 OR y00 -> psh";
    "bqk OR frj -> z08";
    "tnw OR fst -> frj";
    "gnj AND tgd -> z11";
    "bfw XOR mjb -> z00";
    "x03 OR x00 -> vdt";
    "gnj AND wpb -> z02";
    "x04 AND y00 -> kjc";
    "djm OR pbm -> qhw";
    "nrd AND vdt -> hwm";
    "kjc AND fst -> rvg";
    "y04 OR y02 -> fgs";
    "y01 AND x02 -> pbm";
    "ntg OR kjc -> kwq";
    "psh XOR fgs -> tgd";
    "qhw XOR tgd -> z09";
    "pbm OR djm -> kpj";
    "x03 XOR y03 -> ffh";
    "x00 XOR y04 -> ntg";
    "bfw OR bqk -> z06";
    "nrd XOR fgs -> wpb";
    "frj XOR qhw -> z04";
    "bqk OR frj -> z07";
    "y03 OR x01 -> nrd";
    "hwm AND bqk -> z03";
    "tgd XOR rvg -> z12";
    "tnw OR pbm -> gnj";
]
#load "TextInput/Reader.fsx"
let lines = TextInput.Reader.readInput { TextInput.Reader.readInputOptions with Default = sample2; EmptyLineTerminator = false }
solvePuzzle lines