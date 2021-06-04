#load "Pictures.fsx"
#load "Hangman.fsx"
open System

module Hangman =

    open Pictures
    open Hangman

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


    let mutable currentState : State =
        { init with
              secret = init.secret
              hidden = init.hidden
              picked' = init.picked'
              chances = init.chances }


    while (currentState.chances > 0) do

        // Picture diplayed
        printfn "%A" HANGMANPICS.[currentState.chances]
        printfn "\n(exit: ctrl + C) Enter a letter ?: \n"

        // Input
        let mutable pick = char (System.Console.ReadLine())

        // match pick with 
        // | pick when (List.contains pick currentState.picked') = false 
        //     -> currentState.picked' <- pick :: currentState.picked'

        // | pick when (currentState.secret.Contains(pick)) = false
        //     -> currentState.chances <- currentState.chances - 1
        
        if not (List.contains pick currentState.picked') then
            currentState.picked' <- pick :: currentState.picked'
        if not (currentState.secret.Contains(pick)) then
            currentState.chances <- currentState.chances - 1

        currentState.hidden <-  hideLetters currentState.secret (conversionType currentState.picked')
        printfn "-Hidden: %A\n-Secret* %A\n-Picked %A" currentState.hidden currentState.secret (PresentPickedLetters currentState.picked')

        match (currentState.hidden.Contains("_")) with
        | false ->
            printfn "Answome! You win!"
            printfn "%A" DANCING
            Environment.Exit 55
        | true ->
            printfn "Chances left: %A" currentState.chances

    printfn "%A" FINGER


// TODO: hide the secret!
// TODO: generator de veggies
// TODO: split side effects from pure functions
