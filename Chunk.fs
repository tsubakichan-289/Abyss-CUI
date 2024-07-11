module Chunk

type TileCategory = Yuka | Kabe | Ana | Mizu

let modulo m n =
    let ans = m % n
    if ans < 0 then ans + n else ans

let cate2Emoji c = 
    match c with
    | Yuka -> "\u001b[43m"  + "＝" + "\u001b[0m"
    | Kabe -> "\u001b[100m" + "＋" + "\u001b[0m"
    | Ana  -> "\u001b[40m"  + "　" + "\u001b[0m"
    | Mizu -> "\u001b[44m"  + "～" + "\u001b[0m"

let cate2Play c = 
    match c with
    | Yuka -> "\u001b[43m"  + "😊" + "\u001b[0m"
    | Kabe -> "\u001b[100m" + "😊" + "\u001b[0m"
    | Ana  -> "\u001b[40m"  + "😊" + "\u001b[0m"
    | Mizu -> "\u001b[44m"  + "😊" + "\u001b[0m"

let cate2Enemy c = 
    match c with
    | Yuka -> "\u001b[43m"  + "👻" + "\u001b[0m"
    | Kabe -> "\u001b[100m" + "👻" + "\u001b[0m"
    | Ana  -> "\u001b[40m"  + "👻" + "\u001b[0m"
    | Mizu -> "\u001b[44m"  + "👻" + "\u001b[0m"

let sepFunc (_A : float, _B : float, x : float, y : float)=
    let c = _A - _B + 1.0
    let a = (c + sqrt(c*c - 4.0 *_A)) / 2.0
    let b = (c - sqrt(c*c - 4.0 *_A)) / 2.0
    let n = sqrt(_A * _B)

    match (x > a * 100.0, y > b * 100.0) with
    | (true , false) -> Mizu
    | (false, true ) -> Ana
    | (true , true ) -> match (x > (a + (1.0 - a) / 20.0) * 100.0, y > (b + (1.0 - b) / 20.0) * 100.0) with
                        | (true, true) -> Kabe
                        | _ -> Yuka
    | (false, false) -> match (x < (a / 20.0) * 100.0, y < (b / 20.0) * 100.0) with
                        | (true, true) -> Kabe
                        | _ -> Yuka

let floatToCategory (f1: float,f2: float) = 
    sepFunc (0.201, 0.301, f1, f2)

type Chunk = class
    val chunkID : (int * int)
    val map : array<array<TileCategory>>
    val enemyRate : float
    val private noiseFunc : (int -> int -> float)
    new (mapFunc,noiseFunc , chunk_x, chunk_y, enemyRate) =
        {
            chunkID = (chunk_x, chunk_y)
            map = [|
                        for i: int in chunk_x * 16 .. (chunk_x + 1) * 16 - 1 -> [|
                            for l: int in chunk_y * 16 .. (chunk_y + 1) * 16 - 1 -> if (-3 < i && i < 3 && -3 < l && l < 3) then Yuka else (floatToCategory (mapFunc i l))
                        |]
                    |]
            enemyRate = enemyRate
            noiseFunc = noiseFunc
        }
    
    member this.isBorn x y = 
        let rate = this.noiseFunc x y
        (rate < this.enemyRate) && this.map.[modulo x 16].[modulo y 16] = Yuka
    end