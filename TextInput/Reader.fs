namespace TextInput

module Reader =

    let readLines (textReader: System.IO.TextReader) =
        let rec readLinesRec accumulator =
            match textReader.ReadLine() with
            | null | "" -> accumulator |> List.rev
            | line -> readLinesRec (line :: accumulator)
        readLinesRec []