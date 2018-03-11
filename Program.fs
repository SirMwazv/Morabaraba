// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
module Main 
open ProgramData
open System


[<EntryPoint>]
let main argv =  
    runGame
    Console.ReadLine()
    0 // return an integer exit code
