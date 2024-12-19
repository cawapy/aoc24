#load "TextInput/Reader.fsx"
open TextInput
open System
let sample = [ "Register A: 2024"
               "Register B: 0"
               "Register C: 0"
               ""
               "Program: 0,3,5,4,3,0" ]

let lines = Reader.readInput { Reader.readInputOptions with Default = sample; EmptyLineTerminator = false }

type RegisterBank = { A: int64; B: int64; C: int64; IP: int }
type Program = int list


let parse (lines: string list) =
    let rec _parse (lines: string list) (registers: RegisterBank) program =
        match lines with
        | l :: tail ->
            match l.Split(" ") |> Array.toList with
            | ["Register"; "A:"; a ] -> _parse tail {registers with A = Int32.Parse a } program
            | ["Register"; "B:"; a ] -> _parse tail {registers with B = Int32.Parse a } program
            | ["Register"; "C:"; a ] -> _parse tail {registers with C = Int32.Parse a } program
            | ["Program:"; p ] -> _parse tail registers (p.Split(",") |> Array.map Int32.Parse |> Array.toList)
            | [""] -> _parse tail registers program
            | _ -> failwith $"Invalid input \"{l}\""
        | [] -> (registers, program)
    _parse lines { IP=0; A=0; B=0; C=0; } []

let evalCombo (combo: int) (registers: RegisterBank) =
    match combo with
    | 0 | 1 | 2 | 3 -> int64 combo
    | 4 -> registers.A
    | 5 -> registers.B
    | 6 -> registers.C
    | _ -> failwith "Illegal combo operand 7"

// The adv / bdv / cdv instructions perform division.
// The numerator is the value in the A register.
// The denominator is found by raising 2 to the power of the instruction's combo operand.
// (So, an operand of 2 would divide A by 4 (2^2); an operand of 5 would divide A by 2^B.)
let dv (combo: int) (registers: RegisterBank) =
    let operand = evalCombo combo registers
    let rec denominator (op: int64) (acc: int64) =
        if op = 0 then acc else denominator (op-(int64 1)) (acc*(int64 2))
    registers.A / (denominator operand (int64 1))

// The adv instruction (opcode 0) performs division.
// The result of the division operation is truncated to an integer and then written to the A register.
let adv0 (combo: int) (registers: RegisterBank) (output: int list) =
    let value = dv combo registers
    ({registers with A = value}, output, None)

// The bxl instruction (opcode 1) calculates the bitwise XOR of register B
// and the instruction's literal operand, then stores the result in register B.
let bxl1 (literal: int) (registers: RegisterBank) (output: int list) =
    ({registers with B = registers.B ^^^ literal}, output, None)

// The bst instruction (opcode 2) calculates the value of its combo operand modulo 8
// (thereby keeping only its lowest 3 bits), then writes that value to the B register.
let bst2 (combo: int) (registers: RegisterBank) (output: int list) =
    let operand = evalCombo combo registers
    ({registers with B = operand % (int64 8)}, output, None)

// The jnz instruction (opcode 3) does nothing if the A register is 0.
// However, if the A register is not zero, it jumps by setting the
// instruction pointer to the value of its literal operand;
// if this instruction jumps, the instruction pointer is not increased by 2 after this instruction.
let jnz3 (literal: int) (registers: RegisterBank) (output: int list) =
    let operand = literal
    (registers, output, (if registers.A = 0 then None else Some operand))

// The bxc instruction (opcode 4) calculates the bitwise XOR of register B and register C,
// then stores the result in register B.
// (For legacy reasons, this instruction reads an operand but ignores it.)
let bxc4 (_: int) (registers: RegisterBank) (output: int list) =
    ({registers with B = registers.B ^^^ registers.C}, output, None)

// The out instruction (opcode 5) calculates the value of its combo operand modulo 8,
// then outputs that value. (If a program outputs multiple values, they are separated by commas.)
let out5 (combo: int) (registers: RegisterBank) (output: int list) =
    let value = evalCombo combo registers
    let effective = int (value % (int64 8))
    (registers, effective :: output, None)

// The bdv instruction (opcode 6) works exactly like the adv instruction
// except that the result is stored in the B register.
// (The numerator is still read from the A register.)
let bdv6 (combo: int) (registers: RegisterBank) (output: int list) =
    let value = dv combo registers
    ({registers with B = value}, output, None)

// The cdv instruction (opcode 7) works exactly like the adv instruction
// except that the result is stored in the C register.
// (The numerator is still read from the A register.)
let cdv7 (combo: int) (registers: RegisterBank) (output: int list) =
    let value = dv combo registers
    ({registers with C = value}, output, None)

let decode opcode =
    match opcode with
    | 0 -> adv0
    | 1 -> bxl1
    | 2 -> bst2
    | 3 -> jnz3
    | 4 -> bxc4
    | 5 -> out5
    | 6 -> bdv6
    | 7 -> cdv7
    | _ -> failwith $"Illegal instruction {opcode}"

let run (registers: RegisterBank) (program: Program) =
    let prg = program |> List.toArray
    let rec _run registers output =
        if 0 <= registers.IP && registers.IP+1 < prg.Length then
            let opcode = prg[registers.IP]
            let operand = prg[registers.IP+1]
            let operation = decode opcode
            let executionResult = operation operand registers output
            let (newCpu: RegisterBank, newOutput: int list, jumpTarget: int option) = executionResult
            let newIp = match jumpTarget with | None -> newCpu.IP + 2 | Some ip -> ip
            _run {newCpu with IP = newIp } newOutput
        else output |> List.rev // halt
    _run registers []

let findInitialA (registers: RegisterBank) (program: Program) =
    let rec _findInitialA prog (programSuffix: Program) (aValues: int64 list) =
        match prog with
        | item :: tail ->
            let newProgramSuffix = item :: programSuffix
            let newAValues = List.allPairs aValues [int64 0 .. int64 7] |>
                             List.map (fun (a: int64, i: int64) -> int64 8 * a + i) |>
                             List.where (fun a -> newProgramSuffix = run { registers with A = a } program)
            _findInitialA tail newProgramSuffix newAValues
        | [] -> aValues.Head
    _findInitialA (program |> List.rev) [] [0]

let registers, program = parse lines

let initialA: int64 = findInitialA registers program

printfn $"Initial A is {initialA}"