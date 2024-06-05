module Program

open System
open Game

let rand = Random()

[<EntryPoint>]
let main argv =
    Console.Clear()
    let seed = rand.Next()

    let g = new Game (seed)
    g.setChunks
    g.print

    printf "\u001b[1;34Hｑ　ｗ　ｅ"
    printf "\u001b[2;34H　＼｜／　"
    printf "\u001b[3;34Hａー　ーｄ"
    printf "\u001b[4;34H　／｜＼　"
    printf "\u001b[5;34Hｚ　ｓ　ｃ"

    printf "\u001b[7;34HEsc : quit"
    printf "\u001b[8;34Hseed : %d" seed

    let rec loop b =
        if b
            then
                let key = Console.ReadKey(true)
                if key.Key = ConsoleKey.Escape 
                    then Console.Clear(); loop false
                    else 
                        g.exec key.KeyChar
                        loop b
            else ()

    loop true
    0