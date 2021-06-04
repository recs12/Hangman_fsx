module  Hangman

    // Split word into list of char.
    let splitIntoList text' : List<char> =
        Seq.toList text'


    // Generate list from a word
    let createWordFromList lt' =
        let lt = List.map string lt'
        String.concat " " (Array.ofList (lt))


    let rec wordListWithSpaces word' =
        //["a"; "b" ; "c"] -> ["a "; "b "; "c"]
        match word' with
        | [] -> []
        | [ head ] -> [ head ]
        | x :: xs -> (x + " ") :: wordListWithSpaces xs


    let inline replace list a b =
        list |> Seq.map (fun x -> if x = a then b else x)


    let rec addUnderscores secret' picked' =
        match secret' with
        | [] -> []
        | x :: xs -> (if not (List.contains x picked') then "_" else x ) :: addUnderscores xs picked'


    let conversionType lChars =
        lChars |> List.map (fun x -> string(x))


    let hideLetters secret picked' =
        let secret' = splitIntoList secret
        let secret'' = conversionType secret'
        let display = addUnderscores secret'' picked'
        let word = createWordFromList display
        word


    let PresentPickedLetters picked' =
        let word = createWordFromList picked'
        word