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

type Dungeon = class
    val mutable chunks : Map<(int*int), Chunk>
    val noise : (PerlinNoise*PerlinNoise)
    val seed : int 
    val mutable stage : Character array

    new (seed : int) = {
        seed = seed
        noise = (new PerlinNoise (seed),new PerlinNoise (seed + 88182))
        chunks = Map []
        stage = [||]
    }

    member this.addChunk (chunkX: int) (chunkY: int) =
        let newChunk: Chunk = new Chunk ( 
            match this.noise with
            | (p, q) -> fun x y -> (p.mainMap x y, q.mainMap x y)
        , chunkX, chunkY)
        this.chunks <- this.chunks.Add ((chunkX, chunkY), newChunk)
    
    member this.getTile x y =
        let chunk_x = div x 16
        let chunk_y = div y 16
        let x_chunk = modulo x 16
        let y_chunk = modulo y 16
        
        if not (this.chunks.ContainsKey (chunk_x, chunk_y))
            then this.addChunk chunk_x chunk_y
        let chunk = this.chunks.[(chunk_x, chunk_y)]
        (*if x_chunk >= 0 && x_chunk < chunk.map.Length && y_chunk >= 0 && y_chunk < chunk.map.[x_chunk].Length 
            then
                chunk.map.[x_chunk].[y_chunk]
            else
                failwithf "Invalid tile coordinates (%d, %d) in chunk (%d, %d)" x_chunk y_chunk chunk_x chunk_y*)
        this.chunks.[(chunk_x, chunk_y)].map.[x_chunk].[y_chunk]
        
    member this.print x y = 
        printf "\u001b[0;0H　　　　　　　　　　１１１１１"
        printf "\u001b[2;0H０１２３４５６７８９０１２３４"
        for i in 0 .. 14 do
            for l in 0 .. 14 do
                printf "\u001b[%d;%dH" (l + 3) (i * 2 + 1)
                printf "%s" (cate2Emoji (this.getTile (x - 7 + i) (y - 7 + l)))
                printf "\u001b[%d;32H%d" (l + 3) l
    end