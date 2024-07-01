module Game

open Dungeon
open System
open Character
open Chunk

type Game(seed: int) =
    let mutable dungeons = [| new Dungeon(seed) |]
    let mutable dungeonNum = 0
    let mutable camX = 0
    let mutable camY = 0
    let mutable player = new Player (10, 0, 0, 0)

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

    member private this.Dungeons
        with get() = dungeons
        and set(value) = dungeons <- value

    member this.Dungeon
        with get() = this.Dungeons.[this.DungeonNum]
        and set(value) = this.Dungeons.[this.DungeonNum] <- value

    member this.DungeonNum
        with get() = dungeonNum
        and set(value) = dungeonNum <- value

    member this.Player
        with get() = player
        and set(value) = player <- value

    member this.print =
        this.Dungeon.print this.CamX this.CamY
        printf "\u001b[%d;%dH%s" 10 15 (cate2Play (this.Dungeon.getTile this.CamX this.CamY)) // プレイヤーの位置を表示
        for c in this.Dungeon.stage do
            let cX = c.X + 7 - this.Player.X
            let cY = c.Y + 10 + this.Player.Y
            if cX >= 0 && cX < 15 && cY >= 3 && cY < 18 then
                printf "\u001b[%d;%dH%s" cY (cX * 2 + 1) (cate2Enemy (this.Dungeon.getTile c.X c.Y))
        printf "\u001b[9;34H"
        match (this.Dungeon.getTile this.CamX this.CamY) with
        | Yuka -> printf "Yuka"
        | Kabe -> printf "Kabe"
        | Mizu -> printf "Mizu"
        | Ana  -> printf "Ana "
        printf " (%d, %d)       " this.Player.X this.Player.Y
    
        member this.move (character: Character) (a:int) (b:int) =
        let tile = this.Dungeon.getTile (character.X + a) (character.Y + b)
        match (tile, Array.exists (fun (p: Character) -> (p.X = character.X + a &&  (- p.Y) = character.Y + b)) (Array.append this.Dungeon.stage [|this.Player|])) with
        | (Yuka,false) -> character.X <- character.X + a; character.Y <- character.Y + b
        | _ ->()

    member this.exec char =
        match char with
        | 'a' -> this.move this.Player (- 1) 0
        | 'q' -> this.move this.Player (- 1) 1
        | 'w' -> this.move this.Player 0     1
        | 'e' -> this.move this.Player 1     1
        | 'd' -> this.move this.Player 1     0
        | 'c' -> this.move this.Player 1     (- 1)
        | 's' -> this.move this.Player 0     (- 1)
        | 'z' -> this.move this.Player (- 1) (- 1)
        | _ -> ()
        for c in this.Dungeon.stage do
            this.move c 0 1
        this.CamX <- this.Player.X
        this.CamY <- - this.Player.Y
        this.print