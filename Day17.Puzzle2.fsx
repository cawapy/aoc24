#load "TextInput/Reader.fsx"
open TextInput
open System
let sample = [ "Register A: 2024"
               "Register B: 0"
               "Register C: 0"
               ""
               "Program: 0,3,5,4,3,0" ]

let lines = Reader.readInput { Reader.readInputOptions with Default = sample; EmptyLineTerminator = false }

type CPU = { A: int; B: int; C: int; IP: int }
type Program = int list


let parse (lines: string list) =
    let rec _parse (lines: string list) (cpu: CPU) program =
        match lines with
        | l :: tail ->
            match l.Split(" ") |> Array.toList with
            | ["Register"; "A:"; a ] -> _parse tail {cpu with A = Int32.Parse a } program
            | ["Register"; "B:"; a ] -> _parse tail {cpu with B = Int32.Parse a } program
            | ["Register"; "C:"; a ] -> _parse tail {cpu with C = Int32.Parse a } program
            | ["Program:"; p ] -> _parse tail cpu (p.Split(",") |> Array.map Int32.Parse |> Array.toList)
            | [""] -> _parse tail cpu program
            | _ -> failwith $"Invalid input \"{l}\""
        | [] -> (cpu, program)
    _parse lines { IP=0; A=0; B=0; C=0; } []

let evalCombo (combo: int) (cpu: CPU) =
    match combo with
    | 0 | 1 | 2 | 3 -> (combo, $"{combo}")
    | 4 -> (cpu.A, "A")
    | 5 -> (cpu.B, "B")
    | 6 -> (cpu.C, "C")
    | _ -> failwith "Illegal combo operand 7"

// The adv / bdv / cdv instructions perform division.
// The numerator is the value in the A register.
// The denominator is found by raising 2 to the power of the instruction's combo operand.
// (So, an operand of 2 would divide A by 4 (2^2); an operand of 5 would divide A by 2^B.)
let dv (combo: int) (cpu: CPU) =
    let operand, opName = evalCombo combo cpu
    let rec denominator op acc =
        if op = 0 then acc else denominator (op-1) (acc*2)
    (cpu.A / (denominator operand 1), opName)

// The adv instruction (opcode 0) performs division.
// The result of the division operation is truncated to an integer and then written to the A register.
let adv0 (combo: int) (cpu: CPU) (output: int list) =
    let value, opName = dv combo cpu
    ({cpu with A = value}, output, None, ("adv", $".{combo}", $"A := A / 2^{opName}"))

// The bxl instruction (opcode 1) calculates the bitwise XOR of register B
// and the instruction's literal operand, then stores the result in register B.
let bxl1 (literal: int) (cpu: CPU) (output: int list) =
    ({cpu with B = cpu.B ^^^ literal}, output, None, ("bxl", $"${literal}", $"B := B XOR {literal}"))

// The bst instruction (opcode 2) calculates the value of its combo operand modulo 8
// (thereby keeping only its lowest 3 bits), then writes that value to the B register.
let bst2 (combo: int) (cpu: CPU) (output: int list) =
    let operand, opName = evalCombo combo cpu
    ({cpu with B = operand % 8}, output, None, ("bst", $".{combo}", $"B := {opName} %% 8"))

// The jnz instruction (opcode 3) does nothing if the A register is 0.
// However, if the A register is not zero, it jumps by setting the
// instruction pointer to the value of its literal operand;
// if this instruction jumps, the instruction pointer is not increased by 2 after this instruction.
let jnz3 (literal: int) (cpu: CPU) (output: int list) =
    let operand = literal
    (cpu, output, (if cpu.A = 0 then None else Some operand), ("jnz", $"${literal}", $"IP := A ? {literal}"))

// The bxc instruction (opcode 4) calculates the bitwise XOR of register B and register C,
// then stores the result in register B.
// (For legacy reasons, this instruction reads an operand but ignores it.)
let bxc4 (_: int) (cpu: CPU) (output: int list) =
    ({cpu with B = cpu.B ^^^ cpu.C}, output, None, ("bxc", " _", "B := B XOR C"))

// The out instruction (opcode 5) calculates the value of its combo operand modulo 8,
// then outputs that value. (If a program outputs multiple values, they are separated by commas.)
let out5 (combo: int) (cpu: CPU) (output: int list) =
    let value, opName = evalCombo combo cpu
    let effective = value % 8
    (cpu, effective :: output, None, ("out", $".{combo}", $"OUTPUT {opName}"))

// The bdv instruction (opcode 6) works exactly like the adv instruction
// except that the result is stored in the B register.
// (The numerator is still read from the A register.)
let bdv6 (combo: int) (cpu: CPU) (output: int list) =
    let value, opName = dv combo cpu
    ({cpu with B = value}, output, None, ("bdv", $".{combo}", $"B := A / 2^{opName}"))

// The cdv instruction (opcode 7) works exactly like the adv instruction
// except that the result is stored in the C register.
// (The numerator is still read from the A register.)
let cdv7 (combo: int) (cpu: CPU) (output: int list) =
    let value, opName = dv combo cpu
    ({cpu with C = value}, output, None, ("cdv", $".{combo}", $"C := A / 2^{opName}"))

let decode opcode =
    match opcode with
    | 0 -> (adv0, "adv")
    | 1 -> (bxl1, "bxl")
    | 2 -> (bst2, "bst")
    | 3 -> (jnz3, "jnz")
    | 4 -> (bxc4, "bxc")
    | 5 -> (out5, "out")
    | 6 -> (bdv6, "bdv")
    | 7 -> (cdv7, "cdv")
    | _ -> failwith $"Illegal instruction {opcode}"

let decodeProgram (program: Program) =
    let rec _decodeProgram program decoded =
        match program with
        | opcode :: operand :: tail ->
            let operation = fst (decode opcode)
            let _, _, _, (name, operand, descr) = operation operand { A = 0; B = 0; C = 0; IP = 0 } []
            _decodeProgram tail ($"{name} {operand}     ; {descr}" :: decoded)
        | [] -> decoded |> List.rev
        | _ -> failwith "Incomplete instruction"
    _decodeProgram program []

let run (cpu: CPU) (program: Program) =
    let prg = program |> List.toArray
    let rec _run cpu output =
        if 0 <= cpu.IP && cpu.IP < prg.Length then
            let opcode = prg[cpu.IP]
            let operand = prg[cpu.IP+1]
            let operation, _ = decode opcode
            // printfn $"{cpu.IP}: {opName} {operand}"
            let executionResult = operation operand cpu output
            let (newCpu: CPU, newOutput: int list, jumpTarget: int option, _) = executionResult
            let newIp = match jumpTarget with | None -> newCpu.IP + 2 | Some ip -> ip
            _run {newCpu with IP = newIp } newOutput
        else output |> List.rev // halt
    _run cpu []

let findInitialA (cpu: CPU) (program: Program) =
    let rec _findInitialA (a:int) =
        if a % 100_000 = 0 then printfn $"trying {(double a) / (double 1_000_000)}M ..."
        let output: int list = run {cpu with A = a} program
        printfn $"? %A{output}      {a}"
        if output = program then a
        else _findInitialA (a+1)
    _findInitialA 0

let cpu, program = parse lines
printfn $"! %A{program}"
let decoded: string list = decodeProgram program
for l in decoded do printfn $"{l}"

// let initialA: int = findInitialA cpu program

// printfn $"Initial A is {initialA}"