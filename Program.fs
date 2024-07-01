module Program

open System
open Game
open Character

let rand = Random()


[<EntryPoint>]
let main argv =
    Console.Clear()
    let seed = rand.Next()
    let g = new Game (seed)

    g.Dungeon.stage <- [| new Enemy("ghost", 10, 0, -1, -2) |]
    //g.Dungeon.stage <- [| for i in 0 .. 9 do for j in 0 .. 9 do yield new Enemy ("ghost", 10, 0, i, j) |]
    g.print

    printf "\u001b[1;34Hｑ　ｗ　ｅ"
    printf "\u001b[2;34H　＼｜／　"
    printf "\u001b[3;34Hａ－　－ｄ"
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