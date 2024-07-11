module Dungeon

open Chunk
open Noise
open System
open Character

let div m n =
    if m < 0 then
        (m - n + 1) / n
    else
        m / n

let modulo m n =
    let ans = m % n
    if ans < 0 then ans + n else ans

// キーを変更する関数
let changeKey (oldKey:int*int) (newKey:int*int) (map: Map<(int * int),'a>) =
    match map |> Map.tryFind oldKey with
    | Some value ->
        map
        |> Map.remove oldKey
        |> Map.add newKey value
    | None -> 
        printfn "%d,%d" (fst oldKey) (snd oldKey)
        map

type Dungeon = class
    val mutable chunks : Map<(int*int), Chunk>
    val noise : (PerlinNoise*PerlinNoise)
    val seed : int 
    val mutable stage : Map<(int*int),Character>
    val mutable activeEnemy : (int*int) array

    new (seed : int) = {
        seed = seed
        noise = (new PerlinNoise (seed),new PerlinNoise (seed + 88182))
        chunks = Map []
        stage = Map []
        activeEnemy = [||]
    }

    member this.addChunk (chunkX: int) (chunkY: int) =
        let newChunk: Chunk = new Chunk ( 
            match this.noise with
                        | (p, q) -> fun x y -> (p.mainMap x y, q.mainMap x y)
            , (new Noise (this.seed)).mainMap  , chunkX, chunkY, 6.0)
        this.chunks <- this.chunks.Add ((chunkX, chunkY), newChunk)
        for i in 0 .. 15 do
            for l in 0 .. 15 do
                if this.chunks.[(chunkX, chunkY)].isBorn (chunkX * 16 + i) (chunkY * 16 + l)
                    then this.stage <- this.stage.Add ((chunkX * 16 + i, chunkY * 16 + l), new Enemy ("goast", 10, 1, chunkX * 16 + i, chunkY * 16 + l, aStarAI))
                    else ()

    member this.getTile x y =
        let chunk_x = div x 16
        let chunk_y = div y 16
        let x_chunk = modulo x 16
        let y_chunk = modulo y 16
        
        if not (this.chunks.ContainsKey (chunk_x, chunk_y))
            then this.addChunk chunk_x chunk_y
        let chunk = this.chunks.[(chunk_x, chunk_y)]
        this.chunks.[(chunk_x, chunk_y)].map.[x_chunk].[y_chunk]
        
    member this.setTile x y tile =
        let chunk_x = div x 16
        let chunk_y = div y 16
        let x_chunk = modulo x 16
        let y_chunk = modulo y 16
        
        if not (this.chunks.ContainsKey (chunk_x, chunk_y))
            then this.addChunk chunk_x chunk_y
        this.chunks.[(chunk_x, chunk_y)].map.[x_chunk].[y_chunk] <- tile

    member this.print x y = 
        printf "\u001b[0;0H　　　　　　　　　　１１１１１"
        printf "\u001b[2;0H０１２３４５６７８９０１２３４"
        let mutable activeEnemy' = []
        for i in 0 .. 14 do
            for l in 0 .. 14 do
                let xi = x - 7 + i
                let yl = y - 7 + l
                printf "\u001b[%d;%dH" (l + 3) (i * 2 + 1)
                printf "%s" (cate2Emoji (this.getTile xi yl))
                printf "\u001b[%d;32H%d" (l + 3) l
                if this.stage.ContainsKey (xi, yl) then
                    printf "\u001b[%d;%dH%s" (l + 3) (i * 2 + 1) (cate2Enemy (this.getTile xi yl))
                    activeEnemy' <- (xi, yl) :: activeEnemy'
                    else ()
        this.activeEnemy <- activeEnemy' |> List.toArray
    end