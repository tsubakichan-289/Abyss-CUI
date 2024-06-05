module Game

open Dungeon
open System

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

type Game(seed: int) as this =
    let mutable dungeons = [| new Dungeon(seed) |]
    let mutable dungeonNum = 0
    let mutable camX = 0
    let mutable camY = 0
    let mutable chunks = [||]

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
