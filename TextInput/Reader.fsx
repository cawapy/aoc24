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