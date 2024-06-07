module Game

open Dungeon
open System
open Player
open Chunk

let div m n =
    let ans = m / n
    if m < 0
        then ans - 1
        else ans

let modulo m n =
    let ans = m % n
    if m < 0
        then ans + n
        else ans



type Game(seed: int) =
    let mutable dungeons = [| new Dungeon(seed) |]
    let mutable dungeonNum = 0
    let mutable camX = 0
    let mutable camY = 0
    let mutable player = new Player (10, 0, 0, 0, 0)

    // 初期化コード
    do
        dungeons.[0].addChunk 0 0

    // パブリックプロパティの定義
    member val Seed = seed with get, set

    member this.CamX
        with get() = camX
        and set(value) = camX <- value

    member this.CamY
        with get() = camY
        and set(value) = camY <- value

    member this.Dungeons
        with get() = dungeons
        and set(value) = dungeons <- value

    member this.DungeonNum
        with get() = dungeonNum
        and set(value) = dungeonNum <- value

    member this.Player
        with get() = player
        and set(value) = player <- value

    member this.print =
        this.Dungeons.[this.DungeonNum].print this.CamX this.CamY
        printf "\u001b[%d;%dH%s" 10 15 (cate2Play (this.Dungeons.[this.DungeonNum].getTile this.Player.x this.Player.y))
        printf "\u001b[9;34H"
        match (this.Dungeons.[this.DungeonNum].getTile this.Player.x this.Player.y) with
        | Yuka -> printf "Yuka"
        | Kabe -> printf "Kabe"
        | Mizu -> printf "Mizu"
        | Ana  -> printf "Ana "
        printf " (%d, %d)       " this.Player.x this.Player.y

    
    member this.exec char =
        match char with
        | 'a' -> this.Player.x <- this.Player.x - 1
        | 'q' -> this.Player.x <- this.Player.x - 1; this.Player.y <- this.Player.y + 1
        | 'w' ->                                     this.Player.y <- this.Player.y + 1
        | 'e' -> this.Player.x <- this.Player.x + 1; this.Player.y <- this.Player.y + 1
        | 'd' -> this.Player.x <- this.Player.x + 1
        | 'c' -> this.Player.x <- this.Player.x + 1; this.Player.y <- this.Player.y - 1
        | 's' ->                                     this.Player.y <- this.Player.y - 1
        | 'z' -> this.Player.x <- this.Player.x - 1; this.Player.y <- this.Player.y - 1
        | _ -> ()
        this.CamX <- this.Player.x
        this.CamY <- - this.Player.y
        this.print