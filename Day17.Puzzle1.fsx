#load "TextInput/Reader.fsx"
open TextInput
open System
let sample = [ "Register A: 729"
               "Register B: 0"
               "Register C: 0"
               ""
               "Program: 0,1,5,4,3,0" ]

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
    | 0 | 1 | 2 | 3 -> combo
    | 4 -> cpu.A
    | 5 -> cpu.B
    | 6 -> cpu.C
    | _ -> failwith "Illegal combo operand 7"

// The adv / bdv / cdv instructions perform division.
// The numerator is the value in the A register.
// The denominator is found by raising 2 to the power of the instruction's combo operand.
// (So, an operand of 2 would divide A by 4 (2^2); an operand of 5 would divide A by 2^B.)
let dv (combo: int) (cpu: CPU) =
    let operand = evalCombo combo cpu
    let rec denominator op acc =
        if op = 0 then acc else denominator (op-1) (acc*2)
    cpu.A / (denominator operand 1)

// The adv instruction (opcode 0) performs division.
// The result of the division operation is truncated to an integer and then written to the A register.
let adv0 (combo: int) (cpu: CPU) (output: int list) =
    ({cpu with A = dv combo cpu}, output, None)

// The bxl instruction (opcode 1) calculates the bitwise XOR of register B
// and the instruction's literal operand, then stores the result in register B.
let bxl1 (literal: int) (cpu: CPU) (output: int list) =
    ({cpu with B = cpu.B ^^^ literal}, output, None)

// The bst instruction (opcode 2) calculates the value of its combo operand modulo 8
// (thereby keeping only its lowest 3 bits), then writes that value to the B register.
let bst2 (combo: int) (cpu: CPU) (output: int list) =
    let operand = evalCombo combo cpu
    ({cpu with B = operand % 8}, output, None)

// The jnz instruction (opcode 3) does nothing if the A register is 0.
// However, if the A register is not zero, it jumps by setting the
// instruction pointer to the value of its literal operand;
// if this instruction jumps, the instruction pointer is not increased by 2 after this instruction.
let jnz3 (literal: int) (cpu: CPU) (output: int list) =
    let operand = literal
    (cpu, output, if cpu.A = 0 then None else Some operand)

// The bxc instruction (opcode 4) calculates the bitwise XOR of register B and register C,
// then stores the result in register B.
// (For legacy reasons, this instruction reads an operand but ignores it.)
let bxc4 (_: int) (cpu: CPU) (output: int list) =
    ({cpu with B = cpu.B ^^^ cpu.C}, output, None)

// The out instruction (opcode 5) calculates the value of its combo operand modulo 8,
// then outputs that value. (If a program outputs multiple values, they are separated by commas.)
let out5 (combo: int) (cpu: CPU) (output: int list) =
    let operand = (evalCombo combo cpu) % 8
    (cpu, operand :: output, None)

// The bdv instruction (opcode 6) works exactly like the adv instruction
// except that the result is stored in the B register.
// (The numerator is still read from the A register.)
let bdv6 (combo: int) (cpu: CPU) (output: int list) =
    ({cpu with B = dv combo cpu}, output, None)

// The cdv instruction (opcode 7) works exactly like the adv instruction
// except that the result is stored in the C register.
// (The numerator is still read from the A register.)
let cdv7 (combo: int) (cpu: CPU) (output: int list) =
    ({cpu with C = dv combo cpu}, output, None)


let run (cpu: CPU) (program: Program) =
    let prg = program |> List.toArray
    let rec _run cpu output =
        if 0 <= cpu.IP && cpu.IP < prg.Length then
            let opcode = prg[cpu.IP]
            let operand = prg[cpu.IP+1]
            let operation, _ = match opcode with
                                        | 0 -> (adv0, "adv")
                                        | 1 -> (bxl1, "bxl")
                                        | 2 -> (bst2, "bst")
                                        | 3 -> (jnz3, "jnz")
                                        | 4 -> (bxc4, "bxc")
                                        | 5 -> (out5, "out")
                                        | 6 -> (bdv6, "bdv")
                                        | 7 -> (cdv7, "cdv")
                                        | _ -> failwith $"Illegal instruction {opcode}"
            // printfn $"{cpu.IP}: {opName} {operand}"
            let executionResult = operation operand cpu output
            let (newCpu: CPU, newOutput: int list, jumpTarget: int option) = executionResult
            let newIp = match jumpTarget with | None -> newCpu.IP + 2 | Some ip -> ip
            _run {newCpu with IP = newIp } newOutput
        else output |> List.rev // halt
    _run cpu []

let cpu, program = parse lines
let result = run cpu program

let formatted = String.Join(",", result)
printfn $"Output: {formatted}"