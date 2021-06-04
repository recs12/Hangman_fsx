#load "Pictures.fsx"
open System

module Hangman =

    open Pictures

    type State =
        { mutable hidden: string
          mutable secret: string
          mutable picked': List<char>
          mutable chances : int}


    let init =
        { hidden = ""
          secret = "coconut"
          picked' = []
          chances = 12}


    // Split word into list of char.
    let splitIntoList text' : List<char> = Seq.toList text'


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


    let transformationType lChars =
        lChars |> List.map (fun x -> string(x))


    let HideLetters secret picked' =
        let secret' = splitIntoList secret
        let secret'' = transformationType secret'
        let display = addUnderscores secret'' picked'
        let word = createWordFromList display
        word


    let PresentPickedLetters picked' =
        let word = createWordFromList picked'
        word


    let mutable CurrentState : State =
        { init with
              secret = init.secret
              hidden = init.hidden
              picked' = init.picked' 
              chances = init.chances }


    while (CurrentState.chances > 0) do
        printfn "%A" HANGMANPICS.[CurrentState.chances]
        printfn "\n(exit: ctrl + C) Enter a letter ?: \n"
        let mutable x = char (System.Console.ReadLine())
        if not (List.contains x CurrentState.picked') then
            CurrentState.picked' <- x :: CurrentState.picked'
        if not (CurrentState.secret.Contains(x)) then 
            CurrentState.chances <- CurrentState.chances - 1

        CurrentState.hidden <-  HideLetters CurrentState.secret (transformationType CurrentState.picked')
        printfn "-Hidden: %A\n-Secret* %A\n-Picked %A" CurrentState.hidden CurrentState.secret (PresentPickedLetters CurrentState.picked')

        match (CurrentState.hidden.Contains("_")) with
        | false ->
            printfn "Answome! You win!"
            printfn "%A" winner
            Environment.Exit 55
        | true ->
            printfn "Chances left: %A" CurrentState.chances
    printfn "%A" finger


// TODO: hide the secret!
// TODO: drawing of hangman
// TODO: generator de veggies
// TODO: split side effects from pure functions
