module Character
open System.Collections.Generic
type Node = {
    X: int
    Y: int
    G: int
    H: int
    F: int
    Parent: Node option
}

let heuristic (x1: int, y1: int) (x2: int, y2: int) =
    abs(x1 - x2) + abs(y1 - y2)

let isValid (x: int, y: int) (map: int array array) =
    x >= 0 && y >= 0 && x < Array.length map && y < Array.length map.[0] && map.[x].[y] = 0

let aStar (startX: int, startY: int) (goalX: int, goalY: int) (map: int array array) =
    let openList = new SortedSet<Node>(Comparer<Node>.Create(fun a b -> compare a.F b.F))
    let closedList = new HashSet<Node>()
    let startNode = { X = startX; Y = startY; G = 0; H = heuristic (startX, startY) (goalX, goalY); F = heuristic (startX, startY) (goalX, goalY); Parent = None }
    openList.Add(startNode) |> ignore

    let directions = [(-1, -1); (0, -1); (1, -1); (-1, 0); (1, 0); (-1, 1); (0, 1); (1, 1)]

    let rec reconstructPath node =
        match node.Parent with
        | Some parent -> reconstructPath parent
        | None -> (node.X, node.Y)

    let rec findPath () =
        if openList.Count = 0 then
            None
        else
            let currentNode = openList.Min
            openList.Remove(currentNode) |> ignore
            closedList.Add(currentNode) |> ignore

            if currentNode.X = goalX && currentNode.Y = goalY then
                Some (reconstructPath currentNode)
            else
                for (dx, dy) in directions do
                    let newX = currentNode.X + dx
                    let newY = currentNode.Y + dy
                    if isValid(newX, newY) map then
                        let gScore = currentNode.G + 1
                        let hScore = heuristic (newX, newY) (goalX, goalY)
                        let fScore = gScore + hScore
                        let newNode = { X = newX; Y = newY; G = gScore; H = hScore; F = fScore; Parent = Some currentNode }

                        if not (closedList.Contains(newNode)) then
                            let existingNode = openList |> Seq.tryFind (fun n -> n.X = newX && n.Y = newY)
                            match existingNode with
                            | Some node when node.G > gScore ->
                                openList.Remove(node) |> ignore
                                openList.Add(newNode) |> ignore
                            | None ->
                                openList.Add(newNode) |> ignore

                findPath ()

    findPath ()

let aStarAI (map: int array array) (startX: int, startY: int) : int * int =
    let goalX, goalY = 7, 7
    match aStar (startX, startY) (goalX, goalY) map with
    | Some (nextX, nextY) -> (nextX, nextY)
    | None -> (startX, startY) // ゴールに到達できない場合は現在位置を返す

type Direction = 
  | Up
  | Down
  | Left
  | Right
  | UpLeft
  | UpRight
  | DownLeft
  | DownRight

type Character(name: string, health: int, level: int, x: int, y: int, ai: (int array array -> (int*int -> int*int))) =
  member val Name = name with get, set
  member val Health = health with get, set

  member val Level = level with get, set

  member val X = x with get, set
  member val Y = y with get, set
  member val Direction = Down with get, set
  member val AI = ai with get


type Enemy(name: string, health: int, level: int, x: int, y: int, ai: (int array array -> (int*int -> int*int))) =
  inherit Character(name, health, level, x, y, ai)

type Player(health: int, level: int, x: int, y: int) =
  inherit Character("", health, level, x, y, fun _ ->(fun _ -> (0, 0)))
  member this.Exp = 0
  member this.Gold = 0

let someEnemy = new Enemy("ghost", 10, 1, 3, 0, aStarAI)
