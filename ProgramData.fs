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

//Function that returns Position list to hold all possible moves from a position 
let getAdjacentPositions = function
    | A1 -> [D1; A4; B2]
    | A4 -> [A1; B4; A7]
    | A7 -> [A4; B6; D7]
    
    | B2 -> [A1; D2; C3; B4]
    | B4 -> [B2; A4; C4; B6]
    | B6 -> [B4; C5; D6; A7]
    
    | C3 -> [B2; C4; D3]
    | C4 -> [C3; B4; C5]
    | C5 -> [C4; D5; B6]
    
    | D1 -> [A1; G1; D2]
    | D2 -> [D1; F2; D3; B2]
    | D3 -> [D2; E3; C3]

    | D5 -> [E5; D6; C5]
    | D6 -> [D5; F6; D7; B6]
    | D7 -> [D6; G7; A7]
    
    | E3 -> [F2; E4; D3]
    | E4 -> [E3; F4; E5]
    | E5 -> [E4; F6; D5]

    | F2 -> [G1; F4; E3;D2]
    | F4 -> [F2; G4; F6;E4]
    | F6 -> [F4; G7; D6;E5]
   
    | G1 -> [D1; G4; F2]
    | G4 -> [G1; F4; G7]
    | G7 -> [G4; F6; D7]

    |_ -> failwith "No such position"

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
/// Generic Print error function that prints msg to console
/// </summary>
/// <param name="msg">Message to print</param>
let printErr msg  =
    Console.WriteLine(sprintf "Error!:\t{%s}" msg)

/// <summary>
/// Helper function to get players opponent
/// </summary>
/// <param name="player">Current player</param>
/// <param name="state">Current GameState</param>
let getOpponent player state = 
    let opponent =
        match player.Color = state.Player1.Color with
        | true -> state.Player2
        |_ -> state.Player1    
    opponent

/// <summary>
/// Function to extract position from cow union
/// </summary>
let getPos = function 
    | Onboard (_,pos) -> pos
    | Flying (_,pos) -> pos

/// <summary>
/// Helper function that extracts a list of positions from a list of cows
/// </summary>
/// <param name="cows">List of cows to get positons from </param>
let getPosFromCows (cows: Cow List) : Position List =
    List.map getPos cows

/// <summary>
/// function to make sure input is in correct format
/// </summary>
/// <param name="str">input to validate</param>
/// <param name="phase">phase of the game</param>
let isValidInput str phase : bool =
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
/// Function to print the board based on game state 
/// </summary>
/// <param name="state">Current state of the game</param>
let printBoard (state:GameState) =
    Console.Clear()

    //get cows from both players
    let cowsGridList = 
       List.map (fun cow -> cow, getPos cow) (state.Player1.Cows @ state.Player2.Cows)
   
    /// <summary>Helper Function that picks colour based on who's cow the </summary>
    /// <param name = "position">The position who's color to get </param>
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
           ) cowsGridList
       defaultArg col System.ConsoleColor.DarkMagenta
    
    /// <summary>User defined operator that prints text in default console colour</summary>
    /// <param name = "str">String to print to console </param>
    let (~-.) str =
       Console.ResetColor ()
       printf str
    /// <summary>User defined operator that prints a position in appropriate console colour for that position</summary>
    /// <param name = "pos">Position to print to console </param>
    let (~+.) pos = 
       Console.ForegroundColor <- myColor pos
       printf "%A" pos    

    //print out board line by line based on gamestate and player positions 
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

/// <summary>
/// Function to make sure that the move given is valid and check if position is already taken
/// </summary>
/// <param name="inputPos">Move that is being made</param>
/// <param name="player">Player making the move</param>
/// <param name="opponent">Opponent of player</param>
let isValidMove inputPos player opponent = 
        match inputPos with //check if position entered is valid 
        | XX -> false
        | _ ->
            let allPlayerCows = getPosFromCows player.Cows      //get positions of all of players cows
            let allOponentCows = getPosFromCows opponent.Cows   //get positions of opponents cows
            let isMyPosTaken = (List.exists (fun x -> inputPos = x) allPlayerCows)      //check if player already has this position
            let isOpponentPosTaken = (List.exists (fun x -> inputPos = x) allOponentCows)//check if opponent already has position 
            match isMyPosTaken || isOpponentPosTaken with    //check if spot is taken
            | true -> false
            | _ -> true

/// <summary>
/// Function to determine if game should move to next phase
/// </summary>
/// <param name="state"></param>
let checkPhase state = 
    match (state.phase) with
    | Placing ->
            let Player1 = state.Player1
            let Player2 = state.Player2
            let PlacedPlayer1 = Player1.PlacedCows
            let PlacedPlayer2 = Player2.PlacedCows
            match PlacedPlayer1 with
            | 12 -> match PlacedPlayer2 with 
                    |12 -> true
                    |_ -> false
            | _ -> false
    | Moving -> false
    | _ -> false

/// <summary>
/// Helper function to get all possible mills involving the given position 
/// </summary>
/// <param name="pos">position to get mills for</param>
let getPossibleMills pos =
    List.filter (fun (x,y,z) -> 
                        match pos = x with 
                        | true -> true
                        |_ ->
                            match pos = y with 
                            | true -> true
                            |_ ->
                                match pos = z with 
                                | true -> true
                                |_ -> false
    ) millCombos

/// <summary>
/// Helper function to check if players cow is in a mill that they own 
/// </summary>
/// <param name="pos">Position of cow in possible mill</param>
/// <param name="cows">List of players cows</param>
let inMill (pos: Position ) (cows : Cow List) = 
    let cows = getPosFromCows cows
    let filterMills = getPossibleMills pos          //filter all mill combos and get list of mills with the position i'm looking for 
    let myMills = List.exists (fun (x,y,z) ->          //iterate through possible mills and check if player owns cows in all those positions 
                            match 
                                List.exists (fun a -> x = a) cows &&
                                List.exists (fun a -> y = a) cows &&
                                List.exists (fun a -> z = a) cows with                            
                                | true -> true
                                | _ -> false
                    ) filterMills
    myMills

/// <summary>
/// Helper function to fly a players cows and return that cow list
/// </summary>
/// <param name="player"></param>
let flyCows player =
    List.map (fun x ->
                    match x with 
                    | Onboard (x,y) -> Flying (x,y)
                    | _ -> x
    ) player.Cows

/// <summary>
/// Helper function to determine if player has flying cows
/// </summary>
/// <param name="player"></param>
let isFlying player = 
    List.exists (fun x ->
                    match x with 
                    | Flying _ -> true
                    | _ -> false
    ) player.Cows

/// <summary>
/// Function to shoot cows 
/// </summary>
/// <param name="player">Player who is the shooter</param>
/// <param name="state">Current state of the game</param>
let rec shootCow player state  = 
    let shooter = player    
    let shootee =       //determine player getting shot
        match shooter.Color = state.Player1.Color with 
        | true -> state.Player2
        | _ -> state.Player1
    Console.WriteLine(sprintf "Mill! %s please choose which cow you want to shoot" shooter.Alias)    //prompt for input 
    let input = Console.ReadLine().ToUpper()
    let inputPos = strToPos input
    (match isValidInput input Placing with    //validate input (conditions for shooting are the same as in 'Placing' phase)
    |true -> state
    | _ -> Console.WriteLine("Invalid Move!! Please type in a correct grid position as indicated above.")    //if input invalid show error message
           shootCow player state) |> ignore

    let isShootable =   //value to show if cow is shootable (not in mill, empty cell or cow belonging to player)
        //checkfor cow in opponent list (implicitly makes sure we dont shoot our own cow)
        let canShoot = List.exists (fun x -> (getPos x) =  (inputPos) ) shootee.Cows            
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
    let updateShooteePlayer =   //update the shootee player's state 
        match increment with    //check if players cows should now fly 
        |9 -> 
            let tmpPlayer = {shootee with Cows = newCows; DeadCows = increment} 
            let flyingCows = flyCows tmpPlayer
            {tmpPlayer with Cows = flyingCows}
        | _ -> {shootee with Cows = newCows; DeadCows = increment}

    match shooter.Color = state.Player1.Color with  //determine which player to update and then return game state 
    | true -> {state with Player2 = updateShooteePlayer}
    | _ -> {state with Player1 = updateShooteePlayer}

/// <summary>
/// Helper Function to swap cows in player list of cows
/// </summary>
/// <param name="oldPos">Position of old cow</param>
/// <param name="newPos">Position of new cow</param>
/// <param name="player">Player who cows belong to</param>
let switchCow oldPos newPos player =

    let oldCow =    //get cow to remove
        match isFlying player with
        |true -> Flying (player.Color,oldPos)
        | _ -> Onboard (player.Color,oldPos)
    let newCow =    //get cow to replace with 
        match isFlying player with
        |true -> Flying (player.Color,newPos)
        | _ -> Onboard (player.Color,newPos)

    let newCowList =
        let rmvCow =
            List.filter (fun x ->       //filter out old position 
                            match x = oldCow with
                            | true -> false
                            | _ -> true
            ) player.Cows
        let addCow = newCow::rmvCow
        addCow
    {player with Cows = newCowList}


/// <summary>
/// Function to check if a mill has been made on the board by the player
/// </summary>
/// <param name="player">Player who is being checked for a mill</param>
/// <param name="state">Current state of the game</param>
let checkMill pos prevPlayerState newPlayerState state =
        (*
            Rules for mills
            --> Mill cannot be reused  1
            --> Mill has to be broken and remade to be used again, can only be used after 1 turn skipped 0
            --> Only One mill can shoot at a time regardless of how many mills were made in one move  1

            Algorithm
            Get list of all mills player had before moving 
            Get list of mills player has after moving 
            Compare the two and if new mills found return true 
        *)
    let possibleMills = getPossibleMills pos        //possible mills with this position
    let playerPos = List.map getPos newPlayerState.Cows //get a list of all positions of the cows the player has AFTER moving 
    let prevMills = prevPlayerState.MyMills     //mills player had before moving 

    let nextMills =                             //mills player has after moving 
        //filter out mills i already have by removing previous mills from all possible mills (from this position)
        let filterMills =
                            match prevMills.IsEmpty with    //if previous mills is empty then just return possible mills
                            | true -> possibleMills
                            | _ -> List.filter (fun x-> 
                                        List.exists (fun y ->
                                                    match x = y with 
                                                    | true -> false 
                                                    | _ -> true) prevMills) possibleMills //mills that i possibly have but havent checked for
        
        let millsIhave = List.filter (fun (x,y,z) ->        //check if i have any of the possible mills and return the list 
                                                match
                                                    List.exists (fun a ->a = x) playerPos &&
                                                    List.exists (fun a ->a = y) playerPos &&
                                                    List.exists (fun a ->a = z) playerPos with
                                                    | true ->  true
                                                    | _ -> false
                                        ) filterMills 
        millsIhave

    let updatedPlayerState = {newPlayerState with MyMills = nextMills @ prevMills}       //update players mills 
    let updatedGameState =      //update game state 
        match state.Player1.Color = newPlayerState.Color with
        | true -> {state with Player1 = updatedPlayerState}
        | _ -> {state with Player2 = updatedPlayerState}

    match nextMills with    //if new mills were found then return true else return false
    | [] -> (false, state)
    | n::_ -> (true, updatedGameState)

//Moving Phase Functions---------------------------------------

/// <summary>
/// Function to move players piece and return updated game state
/// </summary>
/// <param name="player">Player who is making move</param>
/// <param name="state">Current game state</param>
let rec movePiece player state =
    (*
        Rules for Moving:
        --> Player can only move a cow to an adjacent position 1
        --> The only time the first rule can be igonored "unless their cows are flying" 1
        
        Algorithm:
        1.Ask for player input
        2.Validate input 
        3.Apply rules 
        4.Remove previous position from players cow list
        5.Add new position to player cow list 
    *)


    Console.WriteLine(sprintf "%s, Choose a cow you wish to move (In the format of \"(X,Y) (X,Y)\" where (X,Y) denotes a Grid position)" player.Alias)
    let input = Console.ReadLine().ToUpper()

    //validate input
    let state = 
        match isValidInput input state.phase with
        | true -> state 
        | _  -> printErr "Invalid Input! Please choose a correct Grid Position (X,Y) that is free"
                movePiece player state

    //extract positions
    let strFrom = input.[..1]
    let strTo = input.[3..4] 
    let inputPosFrom = strToPos strFrom
    let inputPosTo = strToPos strTo  
    let opponent = getOpponent player state

    //firstly make sure i can actually move this cow
    match List.exists (fun x -> x = inputPosFrom) (getPosFromCows player.Cows) with
    | true -> state
    | _ -> printErr "Invalid Move! You can't move a cow you dont have"
           movePiece player state

    //validate move
    match isValidMove inputPosTo player opponent with 
    | true -> state
    | _ -> 
        printErr "Invalid Move! Please enter a correct Grid Position (X,Y) that is free"
        movePiece player state

    //apply rules 
    let possibleMoves = getAdjacentPositions inputPosFrom   //get possible moves for a non-flying cow 
    let canMove = List.exists (fun x-> x = inputPosTo ) possibleMoves   //check if i can move to this position for non-flying cow 
    let canFly = isFlying player                 //check if players cows are flying
     
    match canMove || canFly with    //make sure the cow can move/fly to new position
    |true -> ()
    | _ -> printErr "Invalid Move! This cow cannot fly :("

    let movedPlayer = switchCow inputPosFrom inputPosTo player  //and finally move the cow
    
    let newState =
        match player.Color = state.Player1.Color with   //update gamestate 
        | true -> {state with Player1 = movedPlayer}
        | _ -> {state with Player2 = movedPlayer}

    printBoard newState

    //check for mills 
    let isMill = checkMill inputPosTo player movedPlayer newState
    match isMill with //if mills were made then shoot cow else return game state 
    | true,nextState -> 
                        match movedPlayer.Color = nextState.Player1.Color with 
                        | true -> shootCow nextState.Player1 nextState 
                        | _ -> shootCow nextState.Player2 nextState 
    | _ -> newState
    
    

/// <summary>
/// Function to run the moving phase of the game 
/// </summary>
/// <param name="state">State of the game</param>
let rec runMovingPhase gameState =
    printBoard gameState    //print initial board state 

    //Player 1's turn 
    let player1turn = (movePiece gameState.Player1 gameState)  //play player 1 
    printBoard player1turn  //update board

    //todo change this to check for draws / wins 
    (match checkPhase player1turn  with //check if game should move to next phase
    | true -> runMovingPhase {player1turn with phase = Moving}
    | _ -> "") |> ignore
    //---End Turn---

    //Player 2's turn 
    let player2turn = (movePiece player1turn.Player2 player1turn) //play player 2
    printBoard player2turn  //update board

    //todo change this to check for draws / wins 
    (match checkPhase player2turn  with //check if game should move to next phase
    | true -> runMovingPhase {player2turn with phase = Moving}
    | _ -> "") |> ignore
    // ---End Turn---

    ""

//---> END Moving Phase Functions---------------------------------------

//Placing Phase Functions---------------------------------------

/// <summary>
/// Function to palce players
/// </summary>
/// <param name="player">Player who is placing cows</param>
/// <param name="state">Current state of the game</param>
let rec placePiece player state = 
    let opponent = getOpponent player state
    Console.WriteLine(sprintf "%s what is your move?" player.Alias) //ask for player move
    let input = Console.ReadLine().ToUpper(); //read line
    let inputPos = strToPos input   //convert input to position type

    //inner function to check for valid input
    match (isValidInput input state.phase) && (isValidMove inputPos player opponent) with
    | false -> printErr "Invalid Move! Please enter a correct Grid Position (X,Y) that is free"
    | _ -> ()

    //update player and game states 
    let newCow = Onboard (player.Color, inputPos)         //create new cow  
    let increment = player.PlacedCows + 1   //increment number of cows placed 
    let updatePlayer = {player with Cows = newCow::player.Cows; PlacedCows = increment} //add cow to players list of cows
    let newState =      //determine which player just made a move and update thier state 
            match state.Player1.Color = player.Color with
            | true -> {state with Player1 = updatePlayer}
            |_ -> {state with Player2 = updatePlayer}        
    printBoard newState //update board with new game state 

    //check for mills 
    let isMill = checkMill inputPos player updatePlayer newState
    match isMill with //if mills were made then shoot cow else return game state 
    | true,nextState -> 
                        match updatePlayer.Color = nextState.Player1.Color with 
                        | true -> shootCow nextState.Player1 nextState 
                        | _ -> shootCow nextState.Player2 nextState 
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
    (match checkPhase player1turn  with //check if game should move to next phase
    | true -> runMovingPhase {player1turn with phase = Moving}
    | _ -> "") |> ignore
    //---End Turn---

    //Player 2's turn 
    let player2turn = (placePiece player1turn.Player2 player1turn) //play player 2
    printBoard player2turn  //update board
    (match checkPhase player2turn  with //check if game should move to next phase
    | true -> runMovingPhase {player2turn with phase = Moving}
    | _ -> "") |> ignore
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
    Console.WriteLine("Player1 Choose your Nickname") 
    let p1Name = Console.ReadLine()
    Console.WriteLine("Player2 Choose your Nickname") 
    let p2Name = Console.ReadLine()
    Console.Clear()

    //Initialize Data
    //Initiate Players and Game state
    let player1 = {Cows = []; MyMills = []; Alias = p1Name; Color = PlayerColor.Dark; PlacedCows = 0;DeadCows=0}
    let player2 = {Cows = []; MyMills = []; Alias = p2Name; Color = PlayerColor.Light; PlacedCows = 0;DeadCows=0}
    let newGame = {GameState.Player1 = player1; Player2 = player2; isDraw = 0; phase = Placing}
    runPlacingPhase newGame

    //tests for placing 
    //let player1 = {Cows = []; MyMills = []; Alias = "1"; Color = PlayerColor.Dark; PlacedCows = 0;DeadCows = 0}
    //let player2 = {Cows = []; MyMills = []; Alias = "2"; Color = PlayerColor.Light; PlacedCows = 0; DeadCows = 0}
    //let newGame = {GameState.Player1 = player1; Player2 = player2; isDraw = 0; phase = Placing}
    //runPlacingPhase newGame
   
    //tests for moving 
    //let player1 = {Cows = [Onboard (Dark,A1);Onboard (Dark,C4);Onboard (Dark,G7);]; MyMills = []; Alias = "1"; Color = PlayerColor.Dark; PlacedCows = 12;DeadCows = 9}
    //let player2 = {Cows = [Onboard (Light,G1);Onboard (Light,E4);Onboard (Light,A7)]; MyMills = []; Alias = "2"; Color = PlayerColor.Light; PlacedCows = 12; DeadCows = 9}
    //let newGame = {GameState.Player1 = player1; Player2 = player2; isDraw = 0; phase = Moving}
    //runMovingPhase newGame
    
    
     
    
    