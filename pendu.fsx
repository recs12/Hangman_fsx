module Hangman =

    type State =
        { mutable hidden: string
          mutable secret: string
          mutable picked': List<char> }
    //add tries = 12 later.


    let init =
        { hidden = ""
          secret = "coconut"
          picked' = [] }


    // Split word into list of char.
    let splitIntoList text' : List<char> = Seq.toList text'


    // Generate list from a word
    let createWordFromList lt' =
        let lt = List.map string lt'
        String.concat " " (Array.ofList (lt))


    // Gives the length of a given list.
    let getLengthFromList lt' : int = List.length lt'


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
        | x :: xs ->
            (if not (List.contains x picked') then
                "_"
             else x )
            :: addUnderscores xs picked'

    let transformationType lChars =
        lChars |> List.map (fun x -> string(x))

    let HideLetters secret picked' =
        let secret' = splitIntoList secret
        let secret'' = transformationType secret'
        let display = addUnderscores secret'' picked'
        let word = createWordFromList display
        word

    let mutable CurrentState : State =
        { init with
              secret = init.secret
              hidden = init.hidden
              picked' = init.picked' }

    let hiddenCharList = CurrentState.hidden
    let mutable playerTries = 12
    while (playerTries > 0) do
        printfn "\n(exit: ctrl + C) Enter a letter ?: \n"
        let mutable x = char (System.Console.ReadLine())
        if not (List.contains x CurrentState.picked') then
            CurrentState.picked' <- x :: CurrentState.picked'
        CurrentState.hidden <-  HideLetters CurrentState.secret (transformationType CurrentState.picked')
        playerTries <- playerTries - 1
        printfn "-Hidden: %A\n-Secret %A\n-Picked %A" CurrentState.hidden CurrentState.secret CurrentState.picked'
        printfn "Chances left: %A" playerTries
        // 0 |> ignore
    printfn "You have exeeded 12 tries!"

// TODO: stop the game when the word is unhide!
// TODO: hide the secret!
// TODO: hide picked!
// TODO: App pick a random veggy!