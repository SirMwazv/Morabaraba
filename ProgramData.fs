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

type GameState = {
    CurrentPlayer : Player
    NextPlayer : Player
    isDraw : int
}

let findPos x = 
    match x with 
    | Onboard (_,pos) -> pos
    | Flying (_,pos) -> pos

let board (state:GameState) =
    let cowsGridList = 
       List.map (fun cow -> cow, findPos cow) (state.CurrentPlayer.Cows @ state.NextPlayer.Cows)
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





     (* +.A7; -."----------" +D7; -."----------" +G7 
     -."| `.        |         ,' | 
     |   b6------d6------f6   | 
     |   | `.     |    ,' |   | 
     |   |   c5--d5--e5   |   | 
     |   |   |        |   |   | 
     a4--b4--c4      e4--f4--g4 
     |   |   |        |   |   | 
     |   |   c3--d3--e3   |   | 
     |   | ,'    |     `. |   | 
     |   b2------d2------f2   | 
     | ,'         |        `. | 
     a1----------d1----------g1  " *)
