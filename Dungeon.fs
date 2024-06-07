module Dungeon

open Chunk
open Noise
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

type Dungeon = class
    val mutable chunks : Map<(int*int), Chunk>
    val noise : (PerlinNoise*PerlinNoise)
    val seed : int 

    new (seed : int) = {
        seed = seed
        noise = (new PerlinNoise (seed),new PerlinNoise (seed + 88182))
        chunks = Map []
    }

    member this.isIndex (a: int) (b: int) =
        Map.containsKey (a, b) this.chunks

    member this.addChunk (chunkX: int) (chunkY: int) =
        for a in 0 .. 1 do
            for b in 0 .. 1 do
                if this.isIndex (chunkX + a) (chunkY + b)
                    then ()
                    else 
                        let newChunk: Chunk = new Chunk ( 
                            match this.noise with
                            | (p, q) -> fun x y -> (p.mainMap x y, q.mainMap x y)
                        , chunkX + a, chunkY + b)
                        this.chunks <- this.chunks.Add ((chunkX + a, chunkY + b), newChunk)
    
    member this.getTile x y =
        let chunk_x = div x 16
        let chunk_y = div y 16
        let x_chunk = modulo x 16
        let y_chunk = modulo y 16
        
        if this.isIndex (chunk_x - 1) (chunk_y - 1)
            then ()
            else this.addChunk (chunk_x - 1) (chunk_y - 1)

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