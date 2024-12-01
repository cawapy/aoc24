namespace TextInput

module Reader =

    let readLines (textReader: System.IO.TextReader) =
        let rec readLinesRec accumulator =
            let line = textReader.ReadLine()
            match line with
            | null | "" -> accumulator |> List.rev
            | _ -> readLinesRec (line :: accumulator)
        readLinesRec []