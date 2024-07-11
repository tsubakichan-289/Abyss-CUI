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

    member this.exec char =
        match char with
        | 'a' -> this.move this.Player (- 1) 0    ; this.Player.Direction <- Left
        | 'q' -> this.move this.Player (- 1) (-1) ; this.Player.Direction <- UpLeft
        | 'w' -> this.move this.Player 0     (-1) ; this.Player.Direction <- Up
        | 'e' -> this.move this.Player 1     (-1) ; this.Player.Direction <- UpRight
        | 'd' -> this.move this.Player 1     0    ; this.Player.Direction <- Right
        | 'c' -> this.move this.Player 1     1    ; this.Player.Direction <- DownRight
        | 's' -> this.move this.Player 0     1    ; this.Player.Direction <- Down
        | 'z' -> this.move this.Player (- 1) 1    ; this.Player.Direction <- DownLeft
        | 'k' ->
            let attackCoordination = 
                match this.Player.Direction with
                | Up -> (this.Player.X, this.Player.Y - 1)
                | Down -> (this.Player.X, this.Player.Y + 1)
                | Left -> (this.Player.X - 1, this.Player.Y)
                | Right -> (this.Player.X + 1, this.Player.Y)
                | UpLeft -> (this.Player.X - 1, this.Player.Y - 1)
                | UpRight -> (this.Player.X + 1, this.Player.Y - 1)
                | DownLeft -> (this.Player.X - 1, this.Player.Y + 1)
                | DownRight -> (this.Player.X + 1, this.Player.Y + 1)
            this.Dungeon.setTile (fst attackCoordination) (snd attackCoordination) Yuka
        | _ -> ()
        for i in this.Dungeon.activeEnemy do 
            let nowFrame = [| for i in -7 .. 7 do
                                [| for l in -7 .. 7 do
                                    if (this.Dungeon.getTile (this.Player.X + i) (this.Player.Y + l) = Yuka) || not (this.Dungeon.stage.ContainsKey(this.Player.X + i, this.Player.Y + l))
                                        then 1
                                        else 0 |] |]
            let e = this.Dungeon.stage.[i]
            let (a,b) = e.AI (nowFrame) (e.X - this.Player.X + 7, e.Y - this.Player.Y + 7)
            this.move e (-a) (-b)
        this.CamX <- this.Player.X
        this.CamY <- this.Player.Y
        this.print

    member this.move (character: Character) (a:int) (b:int) =
        let tile = this.Dungeon.getTile (character.X + a) (character.Y + b)
        match (tile, (this.Dungeon.stage.Add ((this.Player.X,this.Player.Y),this.Player)).ContainsKey(character.X + a,character.Y + b)) with
        | (Yuka,false) -> 
            this.Dungeon.stage <- changeKey (character.X, character.Y) (character.X + a, character.Y + b) this.Dungeon.stage
            character.X <- character.X + a; character.Y <- character.Y + b
        | _ -> ()