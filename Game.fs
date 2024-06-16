module Game

open Dungeon
open System
open Player
open Chunk

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
        printf "\u001b[%d;%dH%s" 10 15 (cate2Play (this.Dungeons.[this.DungeonNum].getTile this.CamX this.CamY))
        printf "\u001b[9;34H"
        match (this.Dungeons.[this.DungeonNum].getTile this.CamX this.CamY) with
        | Yuka -> printf "Yuka"
        | Kabe -> printf "Kabe"
        | Mizu -> printf "Mizu"
        | Ana  -> printf "Ana "
        printf " (%d, %d)       " this.Player.x this.Player.y

    
    member this.exec char =
        let dun = this.Dungeons.[this.DungeonNum]
        let a = dun.getTile (this.CamX - 1) (this.CamY)
        let q = dun.getTile (this.CamX - 1) (this.CamY - 1)
        let w = dun.getTile (this.CamX)     (this.CamY - 1)
        let e = dun.getTile (this.CamX + 1) (this.CamY - 1)
        let d = dun.getTile (this.CamX + 1) (this.CamY)
        let c = dun.getTile (this.CamX + 1) (this.CamY + 1)
        let s = dun.getTile (this.CamX)     (this.CamY + 1)
        let z = dun.getTile (this.CamX - 1) (this.CamY + 1)
        match (char,a,q,w,e,d,c,s,z) with
        | ('a',Yuka,_,_,_,_,_,_,_) -> this.Player.x <- this.Player.x - 1
        | ('q',_,Yuka,_,_,_,_,_,_) -> this.Player.x <- this.Player.x - 1; this.Player.y <- this.Player.y + 1
        | ('w',_,_,Yuka,_,_,_,_,_) ->                                     this.Player.y <- this.Player.y + 1
        | ('e',_,_,_,Yuka,_,_,_,_) -> this.Player.x <- this.Player.x + 1; this.Player.y <- this.Player.y + 1
        | ('d',_,_,_,_,Yuka,_,_,_) -> this.Player.x <- this.Player.x + 1
        | ('c',_,_,_,_,_,Yuka,_,_) -> this.Player.x <- this.Player.x + 1; this.Player.y <- this.Player.y - 1
        | ('s',_,_,_,_,_,_,Yuka,_) ->                                     this.Player.y <- this.Player.y - 1
        | ('z',_,_,_,_,_,_,_,Yuka) -> this.Player.x <- this.Player.x - 1; this.Player.y <- this.Player.y - 1
        | _ -> ()
        this.CamX <- this.Player.x
        this.CamY <- - this.Player.y
        this.print