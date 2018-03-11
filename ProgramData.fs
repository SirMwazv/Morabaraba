module ProgramData
open System
type Position =
| A1 | A4 | A7
| B2 | B4 | B6
| C3 | C4 | C5
| D1 | D2 | D3 | D5 | D6 | D7
| E3 | E4 | E5
| F2 | F4 | F6
| G1 | G4 | G7

let strToPos pos =
    match pos with
    | "A1" -> A1
    | "A4" -> A4
    | "A7" -> A7
    | "B2" -> B2
    | "B4" -> B4
    | "B6" -> B6
    | "C3" -> C3
    | "C4" -> C4
    | "C5" -> C5
    | "D1" -> D1
    | "D2" -> D2
    | "D3" -> D3
    | "D5" -> D5
    | "D6" -> D6
    | "D7" -> D7
    | "E3" -> E3
    | "E4" -> E4
    | "E5" -> E5
    | "F2" -> F2
    | "F4" -> F4
    | "F6" -> F6
    | "G1" -> G1
    | "G4" -> G4
    | "G7" -> G7
    | _ -> failwith "Error, This is not a position!"


let millCombos =
    [   // horizontal mills
        A7,D7,G7
        B6,D6,F6
        C5,D5,E5
        A4,B4,C4
        E4,F4,G4
        C3,D3,E3
        B2,D2,F2
        A1,D1,G1
        // vertical mills
        A7,A4,A1
        B6,B4,B2
        C5,C4,C3
        D7,D6,D5
        D3,D2,D1
        E5,E4,E3
        F6,F4,F2
        G7,G4,G1
        // diagonal mills
        A7,B6,C5
        A1,B2,C3
        E5,F6,G7
        E3,F2,G1
    ]

type PlayerColor = 
| Dark
| Light
| Neutral 

let darkColor = System.ConsoleColor.DarkBlue
let darkColorFly = System.ConsoleColor.Blue
let lightColor = System.ConsoleColor.DarkGreen
let lightColorFly = System.ConsoleColor.Green

type Cow =
| Onboard of PlayerColor * Position 
| Flying of PlayerColor * Position
| NotOnBoard of PlayerColor
| Dead of PlayerColor

type Player = {
    Alias : string
    Color : PlayerColor
    Cows : Cow list
    MyMills : (Position*Position*Position) list
}

type Phase =
| Placing 
| Moving 

type GameState = {
    Player1 : Player
    Player2 : Player
    isDraw : int
    phase : Phase
}

let findPos x = 
    match x with 
    | Onboard (_,pos) -> pos
    | Flying (_,pos) -> pos


let printBoard (state:GameState) =
    Console.Clear()
    let cowsGridList = 
       List.map (fun cow -> cow, findPos cow) (state.Player1.Cows @ state.Player2.Cows)
    let myColor position =
       let col =            
           List.tryPick (fun (cow,pos) ->
               match position = pos with
               | true -> match cow with
                           | Onboard (Dark, pos) -> Some darkColor
                           | Onboard (Light, pos) -> Some lightColor
                           | Flying (Dark, pos) -> Some lightColorFly
                           | Flying (Light, pos) -> Some darkColorFly
                           | _ -> None
               | _ -> None
           ) cowsGridList
       defaultArg col System.ConsoleColor.DarkMagenta
    let (~-.) str =
       Console.ResetColor ()
       printf str
    let (~+.) pos = 
       Console.ForegroundColor <- myColor pos
       printf "%A" pos    
    +.A7; -."----------"; +.D7; -."----------"; +.G7
    -."\n| \.        |         /' |"
    -."\n|   "; +.B6; -."------"; +.D6; -."------"; +.F6; -."   |"
    -."\n|   | \.     |    /' |   |"
    -."\n|   |   "; +.C5; -."--"; +.D5; -."--"; +.E5; -."   |   |"
    -."\n|   |   |        |   |   |"
    -."\n"; +.A4; -."--"; +.B4; -."--"; +.C4; -."      "; +.E4; -."--"; +.F4; -."--"; +.G4
    -."\n|   |   |        |   |   |"
    -."\n|   |   "; +.C3; -."--"; +.D3; -."--"; +.E3; -."   |   |"
    -."\n|   | /'    |     \. |   |"
    -."\n|   "; +.B2; -."------"; +.D2; -."------"; +.F2; -."   |"
    -."\n| /'         |        \. |"
    -."\n"; +.A1; -."----------"; +.D1; -."----------"; +.G1
    -."\n"

let runGame =
    //Inner function to repeat game loop
    let innerGame state =   
        let rec playerMove player currentState =     //function that manages each players turn 
            Console.WriteLine(sprintf "%s what is your move?" player.Alias) //ask for player move
            let input = Console.ReadLine(); //read line

            //function to validate input 
            //let isValid str phase : bool =
            //    match phase with  //check if in placing phase
            //    | Placing ->
            //        match (String.length str = 2) with
            //        | true -> 
            //            match Char.IsLetter str.[0] &&  Char.IsDigit str.[1] with
            //            |true -> true
            //            |false -> false
            //        |_ -> false
            //    | Moving ->     //check if in moving phase
            //        match (String.length str = 5) with
            //        | true -> 
            //            match Char.IsLetter str.[0] &&  Char.IsDigit str.[1] && Char.IsLetter str.[3] &&  Char.IsDigit str.[4] with
            //            |true -> true
            //            |_ -> false
            //        |_ -> false
            //    |_ -> false

            let isValid str : bool =
                match String.length str with
                |2 ->
                    match Char.IsLetter str.[0] &&  Char.IsDigit str.[1] with
                        |true -> true
                        |_ -> false
                |5 -> 
                    match Char.IsLetter str.[0] &&  Char.IsDigit str.[1] && Char.IsLetter str.[3] &&  Char.IsDigit str.[4] with
                        |true -> true
                        |_ -> false
                |_ -> false
                   

            //get line of input from console and validate it/print any errors
            let line = 
                match isValid input with     //validate line input 
                //match isValid input currentState.phase with     //validate line input
                | true -> input.ToUpper()
                | _ -> ""   //read input from player
            let printErr =  //print an error IF input is invalid 
                match line with
                | "" -> Console.WriteLine("Invalid Move!! Please type in a correct grid position as indicated above.")    //if input empty show error message
                | _ -> ()
            printErr 

            //update player and game states 
            let newCow = Onboard (player.Color,(strToPos line))         //create new cow    
            let updatePlayer = {player with Cows = newCow::player.Cows} //add cow to players list of cows
            let updateState =           //'update' gamestate 
                match player.Color with 
                | Dark -> {state with Player1 = updatePlayer; Player2 = currentState.Player2}
                | Light -> {state with Player1 = currentState.Player1; Player2 = updatePlayer}
                | _ -> failwith "Critical Error. No Colours here"
            printBoard updateState  //print board with changes 
            let nextTurn () =
                match player.Color with 
                | Dark -> playerMove updateState.Player2 updateState
                | Light -> playerMove updateState.Player1 updateState
                | _ -> failwith "Critical Error. Failed to switch turns."
            nextTurn ()
        playerMove state.Player1 state
    //Setup Player Names 
    //Console.WriteLine("Player1 Choose your Nickname") 
    //let p1Name = Console.ReadLine()
    //Console.WriteLine("Player2 Choose your Nickname") 
    //let p2Name = Console.ReadLine()
    //Console.Clear()
    let player1 = {Cows = []; MyMills = []; Alias = "1"; Color = PlayerColor.Dark}
    let player2 = {Cows = []; MyMills = []; Alias = "2"; Color = PlayerColor.Light}
    //Initiate Players and Game state
    //let player1 = {Cows = []; MyMills = []; Alias = p1Name; Color = PlayerColor.Dark}
    //let player2 = {Cows = []; MyMills = []; Alias = p2Name; Color = PlayerColor.Light}
    let newGame = {GameState.Player1 = player1; Player2 = player2; isDraw = 0; phase = Placing}
    printBoard newGame    
    innerGame newGame
    Console.WriteLine("This should not happen")
    