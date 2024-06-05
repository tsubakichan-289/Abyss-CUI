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
    printf "\u001b[5;34Hｚ　ｘ　ｃ"

    let rec loop b =
        if b
            then
                let key = Console.ReadKey(true)
                if key.Key = ConsoleKey.Escape 
                    then Console.Clear(); loop false
                    else 
                        match key.KeyChar with
                        | 'a' -> g.CamX <- g.CamX + 1
                        | 'q' -> g.CamX <- g.CamX + 1; g.CamY <- g.CamY + 1
                        | 'w' -> g.CamY <- g.CamY + 1
                        | 'e' -> g.CamX <- g.CamX - 1; g.CamY <- g.CamY + 1
                        | 'd' -> g.CamX <- g.CamX - 1
                        | 'c' -> g.CamX <- g.CamX - 1; g.CamY <- g.CamY - 1
                        | 'x' -> g.CamY <- g.CamY - 1
                        | 'z' -> g.CamX <- g.CamX + 1; g.CamY <- g.CamY - 1
                        | _ -> ()
                        g.setChunks
                        g.print
                        printfn " %d,%d" g.CamX g.CamY
                        loop b
            else ()

    loop true
    0