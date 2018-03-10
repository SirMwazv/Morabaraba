// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
module Main 
open ProgramData
open System
let player1 = {Player.Cows = []; MyMills = []; Alias = "Me"; Color = PlayerColor.Dark}
let player2 = {Player.Cows = []; MyMills = []; Alias = "Me"; Color = PlayerColor.Light}

let newGame = {GameState.CurrentPlayer = player1; NextPlayer = player2; isDraw = 0}

[<EntryPoint>]
let main argv =     
    board newGame
    Console.ReadLine()
    0 // return an integer exit code
