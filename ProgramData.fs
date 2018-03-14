module ProgramData
open System

//Discriminated Union to represent each grid position 
type Position =
| A1 | A4 | A7
| B2 | B4 | B6
| C3 | C4 | C5
| D1 | D2 | D3 | D5 | D6 | D7
| E3 | E4 | E5
| F2 | F4 | F6
| G1 | G4 | G7
|XX // denotes invalid position 

/// <summary>
/// Helper function to convert string represenation of grid position to Position data type
/// </summary>
/// <param name="pos">position represented as a string</param>
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
    | _ -> XX

//list of all possible cominations for mills on the board
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

//type representing the player colours 
type PlayerColor = 
| Dark
| Light

//set player Colours 
let darkColor = System.ConsoleColor.Cyan
let darkColorFly = System.ConsoleColor.DarkCyan
let lightColor = System.ConsoleColor.Red
let lightColorFly = System.ConsoleColor.DarkRed

//cow data type
type Cow =
| Onboard of PlayerColor * Position 
| Flying of PlayerColor * Position

//record holding player state
type Player = {
    Alias : string
    Color : PlayerColor
    Cows : Cow list
    MyMills : (Position*Position*Position) list
    PlacedCows : int
    DeadCows : int
}

//Data type to represent each phase of the game
type Phase =
| Placing 
| Moving
| Won of Player
| Draw 

//record holding game state 
type GameState = {
    Player1 : Player
    Player2 : Player
    isDraw : int
    phase : Phase
}

/// <summary>
/// Helper function that extracts a list of positions from a list of cows
/// </summary>
/// <param name="cows">List of cows to get positons from </param>
let getPosFromCows (cows: Cow List) : Position List =
    List.map (fun x ->
                    match x with
                    | Onboard (_,p) -> p
                    | Flying (_,p) -> p
    ) cows

/// <summary>
/// function to validate input in correct format
/// </summary>
/// <param name="str">input to validate</param>
/// <param name="phase">phase of the game</param>
let isValid str phase : bool =
    match phase with  //check if in placing phase
    | Placing ->
        match (String.length str = 2) with
        | true -> 
            match Char.IsLetter str.[0] &&  Char.IsDigit str.[1] with
            |true -> true
            |false -> false
        |_ -> false
    | Moving ->     //check if in moving phase
        match (String.length str = 5) with
        | true -> 
            match Char.IsLetter str.[0] &&  Char.IsDigit str.[1] && Char.IsLetter str.[3] &&  Char.IsDigit str.[4] with
            |true -> true
            |_ -> false
        |_ -> false
    |_ -> false

/// <summary>
/// Function to extract position from cow union
/// </summary>
let findPos = function 
    | Onboard (_,pos) -> pos
    | Flying (_,pos) -> pos

/// <summary>
/// Function to print the board based on game state 
/// </summary>
/// <param name="state">Current state of the game</param>
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

let checkPhase state =
    ()

//Placing Phase Functions---------------------------------------

/// <summary>
/// Function to check if players cow is in a mill
/// </summary>
/// <param name="pos">Position of cow in possible mill</param>
/// <param name="cows">List of players cows</param>
let inMill (pos: Position ) (cows : Cow List) = 
    let cows = getPosFromCows cows
    let myMills = List.map (fun (x,y,z) ->               //iterate through possible mills and check if player owns cows in all those positions 
                            match 
                                List.exists (fun a -> x = a) cows &&
                                List.exists (fun a -> y = a) cows &&
                                List.exists (fun a -> z = a) cows with                            
                                | true -> true
                                | _ -> false
                    ) millCombos
    match List.exists (fun x -> x = true) myMills with 
    |true -> true
    |_ -> false

/// <summary>
/// Function to shoot cows 
/// </summary>
/// <param name="player">Player who is the shooter</param>
/// <param name="state">Current state of the game</param>
let rec shootCow player state  = 
    let shooter = player    
    let shootee =       //determine player getting shot
        match shooter = state.Player1 with 
        | true -> state.Player2
        | _ -> state.Player1
    Console.WriteLine(sprintf "Mill! %s please choose which cow you want to shoot" shooter.Alias)    //prompt for input 
    let input = Console.ReadLine().ToUpper()
    let inputPos = strToPos input
    (match isValid input Placing with    //validate input (conditions for shooting are the same as in 'Placing' phase)
    |true -> state
    | _ -> Console.WriteLine("Invalid Move!! Please type in a correct grid position as indicated above.")    //if input invalid show error message
           shootCow player state) |> ignore

    let isShootable =   //value to show if cow is shootable (not in mill, empty cell or cow belonging to player)
        //checkfor cow in opponent list
        let canShoot = List.exists (fun x -> (findPos x) =  (inputPos) ) shootee.Cows            
        //check if cow is in mill  
        let notInMill = not (inMill inputPos shootee.Cows)
        canShoot && notInMill

    
    (match isShootable with  //check if cow is shootable 
    | false -> Console.WriteLine(sprintf "Error! %s, you Cannot Shoot this cow! Please Choose Another Cow." player.Alias)   //if not shootable display error and retry 
               shootCow player state
    |_ -> state)  |> ignore //else do nothing and carry on...

    //"shoot" the cow (remove it from shootee's list and increment killed cows)
    let newCows = List.filter (fun x ->     //filter the shootee's cows and remove the cow that has been shot 
                        let pos = match x with              //get current cows position 
                                    | Onboard (_,p) -> p
                                    | Flying (_,p) -> p
                        match pos = inputPos with
                        | true -> false
                        | _ -> true
                    ) shootee.Cows

    let increment = player.DeadCows + 1 //increment dead cows for player 
    let updateShooteePlayer = {shootee with Cows = newCows; DeadCows = increment} //update the shootee player's state 

    match shooter = state.Player1 with  //determine which player to update and then return game state 
    | true -> {state with Player2 = updateShooteePlayer}
    | _ -> {state with Player1 = updateShooteePlayer}

/// <summary>
/// Function to check if a mill has been made on the board by the player
/// </summary>
/// <param name="player">Player who is being checked for a mill</param>
/// <param name="state">Current state of the game</param>
let checkMill pos prevPlayerState newPlayerState state = //Note: Modify checkMill func to add new mill to shooter (have to change signature)
        (*
            Rules for mills
            --> Mill cannot be reused 
            --> Mill has to be broken and remade to be used again, can only be used after 1 turn skipped
            --> Only One mill can shoot at a time regardless of how many mills were made in one move 

            Algorithm
            Get list of all mills player had before moving 
            Get list of mills player has after moving 
            Compare the two and if new mills found return true 
        *)
        let playerPos = List.map findPos newPlayerState.Cows //get a list of all positions of the cows the player has 
        let prevMills = prevPlayerState.MyMills     //mills player had before moving 
        let nextMills =                             //mills player has after moving 
            List.filter (fun x -> List.exists (fun y->  //filter out mills i already have 
                                                x=y
                                    ) prevMills
                        ) millCombos  
        let isUsedMill =            //check if mill already exists in players list
            List.exists ( fun x ->
                            List.exists (fun y -> x=y) millCombos
            ) newPlayerState.MyMills        

        let checkForMill = 
                            List.map (fun x ->       //iterate through mill combos and check if a mill exists that a player has made
                                    match x with
                                    |a,b,c -> 
                                        match
                                            List.exists (fun y ->y = a) playerPos &&
                                            List.exists (fun y ->y = b) playerPos &&
                                            List.exists (fun y ->y = c) playerPos with
                                            | true ->  true
                                            | _ -> false                        
                            ) millCombos
    
        let millAlreadyUsed =       //make sure that mills aren't being reused (mills already made previously and mills broken in the last round) 
            //let isAlreadyMade =
            //    match List.map (fun (x,y,z) -> 
            //        List.exists ()
            //    ) player.MyMills with
            true

        match (List.exists (fun x -> x = true) checkForMill) && not millAlreadyUsed with // if a mill has been made then return true else return false 
        | true -> true
        | _ -> false

/// <summary>
/// Function to palce players
/// </summary>
/// <param name="player">Player who is placing cows</param>
/// <param name="state">Current state of the game</param>
let rec placePiece player state = 
    let opponent =
        match player = state.Player1 with
        | true -> state.Player2
        |_ -> state.Player1
    Console.WriteLine(sprintf "%s what is your move?" player.Alias) //ask for player move
    let input = Console.ReadLine().ToUpper(); //read line
    let inputPos = strToPos input

    //inner function to check for valid input
    let checkErr msg =  //print an error IF input is invalid 
                match msg with
                | "" -> 
                    Console.WriteLine("Invalid Move!! Please type in a correct grid position as indicated above.")    //if input empty show error message
                    placePiece player state
                | _ -> match isValid msg state.phase with
                        | true -> state 
                        | _ -> Console.WriteLine("Invalid Move!! Please type in a correct grid position as indicated above.")    //if input invalid show error message  
                               placePiece player state
    (checkErr input) |> ignore  //execute 'input check'

    //inner function to check for valid move 
    let checkMove () = // check if position is already taken 
        let myMove = Onboard (player.Color,inputPos) //my cow
        let myOpponentMove = Onboard (opponent.Color, inputPos) //opponents cow
        match (List.exists (fun x -> myMove = x) player.Cows) && (List.exists (fun x -> myOpponentMove = x) opponent.Cows) with    //check if spot is taken
        | true -> Console.WriteLine("Invalid Move!! Please type in a correct grid position that is free as indicated above.")    //if input invalid show error message  
                  placePiece player state
        | _ -> match inputPos with        //check if position entered is valid 
                | XX -> Console.WriteLine("Invalid Move!! Please type in a correct grid position as indicated above.")    //if input invalid show error message  
                        placePiece player state
                | _ -> state
    checkMove () |> ignore //execute 'valid move' check 

    //update player and game states 
    let newCow = Onboard (player.Color, inputPos)         //create new cow  
    let increment = player.PlacedCows + 1   //increment number of cows placed 
    let updatePlayer = {player with Cows = newCow::player.Cows; PlacedCows = increment} //add cow to players list of cows
    let newState =      //determine which player just made a move and update thier state 
            match state.Player1 with
            | player -> {state with Player1 = updatePlayer}
            |_ -> {state with Player2 = updatePlayer}        
    printBoard newState //update board with new game state 

    //check for mills 
    match checkMill inputPos player updatePlayer newState with 
    | true -> shootCow updatePlayer newState 
    | _ -> newState
 
/// <summary>
/// Function to run the placing phase of the game
/// </summary>
/// <param name="gameState">Curret State of the game</param>
let rec runPlacingPhase gameState =
    printBoard gameState    //print initial board state 

    //Player 1's turn 
    let player1turn = (placePiece gameState.Player1 gameState)  //play player 1 
    printBoard player1turn  //update board
    checkPhase player1turn  //check if game should move to next phase
    //---End Turn---

    //Player 2's turn 
    let player2turn = (placePiece player1turn.Player2 player1turn) //play player 2
    printBoard player2turn  //update board
    checkPhase player2turn  //check if game should move to next phase
    // ---End Turn---

    runPlacingPhase  player2turn   //run next round of placing 

//---> END Placing Phase Functions---------------------------------------



/// <summary>
/// Function to initialize and run the game
/// </summary>
let initializeGame () =
    //Print Title  
    Console.WriteLine("MORABARABA \n
        Press Any Key to Start...
    ")
    Console.ReadKey() |> ignore

    //Setup Player Names 
    //Console.WriteLine("Player1 Choose your Nickname") 
    //let p1Name = Console.ReadLine()
    //Console.WriteLine("Player2 Choose your Nickname") 
    //let p2Name = Console.ReadLine()
    //Console.Clear()

    //Initialize Data
    //Initiate Players and Game state
    //let player1 = {Cows = []; MyMills = []; Alias = p1Name; Color = PlayerColor.Dark; PlacedCows = 0}
    //let player2 = {Cows = []; MyMills = []; Alias = p2Name; Color = PlayerColor.Light; PlacedCows = 0}
    let player1 = {Cows = []; MyMills = []; Alias = "1"; Color = PlayerColor.Dark; PlacedCows = 0;DeadCows = 0}
    let player2 = {Cows = []; MyMills = []; Alias = "2"; Color = PlayerColor.Light; PlacedCows = 0; DeadCows = 0}
    let newGame = {GameState.Player1 = player1; Player2 = player2; isDraw = 0; phase = Placing}
    runPlacingPhase newGame
   
    
    
    
    
     
    
    