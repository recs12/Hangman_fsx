 
open System

module Counter =

    type State = {
        hidden: string;
        secret: string;
        picked: List<char>;
    }


    let init = {
        hidden = "_ _ _ _ _";
        secret = "coconut";
        picked = [];
        }


    type Msg = Try


    let update (msg: Msg) (state: State) : State =
        match msg with
        | Try -> { state with hidden = "_ _ _"; secret = "coco"; picked = [] }


    // Split word into list of char.
    let splitIntoList text : List<char> =
        Seq.toList text


    // Generate list from a word
    let createWordFromList lt =
        //la = System.String.Concat()
        let lt' = List.map string lt
        String.concat " " (Array.ofList(lt'))


    // Gives the length of a given list.
    let getLengthFromList listGiven : int =
        List.length listGiven


    let rec wordListWithSpaces word =
        //["a"; "b" ; "c"] -> ["a "; "b "; "c"]
        match word with
        | [] -> []
        | [head] -> [head]
        | x :: xs -> (x + " ")  :: wordListWithSpaces xs

    let inline replace list a b =
        list |> Seq.map (fun x -> if x = a then b else x)

    let game (letter:string) (secretWord:string) (lettersYouCanPick:List<char>) (lettersAlreadyPicked:List<char>) =
        //do something.
        0

    // List.contains 5 [2..2..10]


    let mutable CurrentState:State = {init with secret = init.secret; hidden = init.hidden; picked = init.picked} 
    let l = CurrentState.hidden
    while (List.contains "_" ) do
        printfn "-Hidden: %A\n-Secret %A\n-Picked %A" CurrentState.hidden CurrentState.secret CurrentState.picked
        printfn "\nPick a new letter ?: "