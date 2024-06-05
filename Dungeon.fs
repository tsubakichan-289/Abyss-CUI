module Dungeon

open Chunk
open Noise

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
    end