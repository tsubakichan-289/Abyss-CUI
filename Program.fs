module Program

open System
open System.Collections.Generic // Add this line
open Game
open Character

let rand = Random()

type Node = {
    X: int
    Y: int
    GCost: int
    HCost: int
    Parent: Node option
}

let calculateHCost (x1: int) (y1: int) (x2: int) (y2: int) =
    abs(x1 - x2) + abs(y1 - y2)

let findPath (startX: int) (startY: int) (goalX: int) (goalY: int) (map: int[][]) =
    let mutable openList = []
    let closedList = new HashSet<Node>()
    let startNode = { X = startX; Y = startY; GCost = 0; HCost = calculateHCost startX startY goalX goalY; Parent = None }
    openList <- List.append openList [startNode]

    let getNeighbors (node: Node) =
        [(-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1)]
        |> List.map (fun (dx, dy) -> (node.X + dx, node.Y + dy))
        |> List.filter (fun (x, y) -> x >= 0 && y >= 0 && x < Array.length map && y < Array.length map.[0] && map.[x].[y] = 0)

    let rec findPathInternal () =
        if List.isEmpty openList then None
        else
            let currentNode = List.minBy (fun n -> n.GCost + n.HCost) openList
            if currentNode.X = goalX && currentNode.Y = goalY then Some currentNode
            else
                openList <- List.filter (fun n -> n <> currentNode) openList
                closedList.Add(currentNode)
                getNeighbors currentNode
                |> List.iter (fun (x, y) ->
                    if not (closedList |> Seq.exists (fun n -> n.X = x && n.Y = y)) then
                        let gCost = currentNode.GCost + 1
                        let hCost = calculateHCost x y goalX goalY
                        let neighbor = { X = x; Y = y; GCost = gCost; HCost = hCost; Parent = Some currentNode }
                        if not (openList |> List.exists (fun n -> n.X = x && n.Y = y && n.GCost <= gCost)) then
                            openList <- List.append openList [neighbor])
                findPathInternal ()
    findPathInternal ()

// 使用例
let map = 
    [| [|0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0|]
       [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0|]
       [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0|]
       [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0|]
       [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0|]
       [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0|]
       [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0|]
       [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0|]
       [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0|]
       [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0|]
       [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0|]
       [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0|]
       [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0|]
       [|0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 1; 1; 1; 0|]
       [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0|]
     |]

let startX, startY = 0, 0
let goalX, goalY = 14, 14
let path = findPath startX startY goalX goalY map

// findPath の先頭の node を得る関数
let getFirstNode path =
    match path with
    | Some node -> Some node
    | None -> None

[<EntryPoint>]
let main argv =
    for line in map do
        for cell in line do
            if cell = 0 then printf " " else printf "■"
        printfn ""
    0

// findPath startNode goalNode map で経路を検索

(*
[<EntryPoint>]
let main argv =
    Console.Clear()
    let seed = rand.Next()
    let g = new Game (seed)

    g.print

    printf "\u001b[1;34Hｑ　ｗ　ｅ"
    printf "\u001b[2;34H　＼｜／　"
    printf "\u001b[3;34Hａ－　－ｄ"
    printf "\u001b[4;34H　／｜＼　"
    printf "\u001b[5;34Hｚ　ｓ　ｃ"

    printf "\u001b[7;34HEsc : quit"
    printf "\u001b[8;34Hseed : %d" seed

    let rec loop b =
        if b
            then
                let key = Console.ReadKey(true)
                if key.Key = ConsoleKey.Escape 
                    then Console.Clear(); loop false
                    else 
                        g.exec key.KeyChar
                        printf "\u001b[1;34Hｑ　ｗ　ｅ"
                        printf "\u001b[2;34H　＼｜／　"
                        printf "\u001b[3;34Hａ－　－ｄ"
                        printf "\u001b[4;34H　／｜＼　"
                        printf "\u001b[5;34Hｚ　ｓ　ｃ"
                        match g.Player.Direction with
                        | Up -> printf "\u001b[1;38H\u001b[31mｗ\u001b[0m"
                        | Down -> printf "\u001b[5;38H\u001b[31mｓ\u001b[0m"
                        | Left -> printf "\u001b[3;34H\u001b[31mａ\u001b[0m"
                        | Right -> printf "\u001b[3;42H\u001b[31mｄ\u001b[0m"
                        | UpLeft -> printf "\u001b[1;34H\u001b[31mｑ\u001b[0m"
                        | UpRight -> printf "\u001b[1;42H\u001b[31mｅ\u001b[0m"
                        | DownLeft -> printf "\u001b[5;34H\u001b[31mｚ\u001b[0m"
                        | DownRight -> printf "\u001b[5;42H\u001b[31mｃ\u001b[0m"
                        loop b
            else ()

    loop true
    0
*)