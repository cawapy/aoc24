namespace TextInput

module Reader =

    let readLines (textReader: System.IO.TextReader) =
        let rec readLinesRec accumulator =
            match textReader.ReadLine() with
            | null | "" -> accumulator |> List.rev
            | line -> readLinesRec (line :: accumulator)
        readLinesRec []

    let rec readLinesOrDefault (textReader: System.IO.TextReader) (_default: string list) =
        printfn "Paste input (or nothing for builtin default), terminate with <CR> on empty line."
        match readLines textReader with
        | [] -> printfn "Using builtin default"; _default
        | x -> printfn "Using provided input"; x

    type ReadInputOptions = { Default: string list; Input: System.IO.TextReader; EmptyLineTerminator: bool }

    let readInput (options: ReadInputOptions ) =
        let rec _readLinesRev accumulator =
            match options.Input.ReadLine() with
            | null -> accumulator
            | "" when options.EmptyLineTerminator -> accumulator
            | line -> _readLinesRev (line :: accumulator)
        if options.Default <> [] then
            if options.EmptyLineTerminator then
                printfn "Paste input (or nothing for builtin default), terminate with EOF or <CR> on empty line."
            else
                printfn "Paste input (or nothing for builtin default), terminate with EOF."
        let input = _readLinesRev [] |> List.rev
        match input with
        | [] -> printfn "Using builtin default"; options.Default
        | x -> printfn "Using provided input"; x

    let readInputOptions = { Input = System.Console.In; EmptyLineTerminator = true; Default = [] }