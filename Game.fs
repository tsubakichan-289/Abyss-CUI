module Game

open Dungeon
open System
open Player

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
    let mutable chunks = [||]
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

    member this.Chunks
        with get() = chunks
        and set(value) = chunks <- value

    member this.Player
        with get() = player
        and set(value) = player <- value

    member this.setChunks =
        let chunkX = -(div this.CamX 16)
        let chunkY = -(div this.CamY 16)
        this.Dungeons.[this.DungeonNum].addChunk chunkX chunkY
        this.Chunks <- [|
            for i in 0 .. 1 do
                yield [|
                    for l in 0 .. 1 do
                        yield this.Dungeons.[this.DungeonNum].chunks.[(chunkX + i,chunkY + l)]
                |]
        |]

    member this.print =
        let chunkX = modulo this.CamX 16
        let chunkY = modulo this.CamY 16
        for i in 0 .. 1 do
            for l in 0 .. 1 do
                this.Chunks.[i].[l].print (i * 16 + chunkX) (l * 16 + chunkY)
        printf "\u001b[%d;%dH😊" 10 15
        printf "\u001b[9;34H"
        match (this.Dungeons.[this.DungeonNum].getTile this.Player.x this.Player.y) with
        | Yuka -> printf "Yuka"
        | Kabe -> printf "Kabe"
        | Mizu -> printf "Mizu"
        | Ana  -> printf "Ana "
        printf " (%d, %d)       " this.Player.x this.Player.y
    
    member this.exec char =
        match char with
        | 'a' -> this.Player.x <- this.Player.x + 1
        | 'q' -> this.Player.x <- this.Player.x + 1; this.Player.y <- this.Player.y + 1
        | 'w' -> this.Player.y <- this.Player.y + 1
        | 'e' -> this.Player.x <- this.Player.x - 1; this.Player.y <- this.Player.y + 1
        | 'd' -> this.Player.x <- this.Player.x - 1
        | 'c' -> this.Player.x <- this.Player.x - 1; this.Player.y <- this.Player.y - 1
        | 's' -> this.Player.y <- this.Player.y - 1
        | 'z' -> this.Player.x <- this.Player.x + 1; this.Player.y <- this.Player.y - 1
        | _ -> ()
        this.CamX <- this.Player.x
        this.CamY <- this.Player.y
        this.setChunks
        this.print