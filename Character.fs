module Character


type Character(name: string, health: int, level: int, x: int, y: int) =
  member val Name = name with get, set
  member val Health = health with get, set

  member val Level = level with get, set

  member val X = x with get, set
  member val Y = y with get, set

type Enemy(name: string, health: int, level: int, x: int, y: int) =
  inherit Character(name, health, level, x, y)

type Player(health: int, level: int, x: int, y: int) =
  inherit Character("", health, level, x, y)

  member this.Exp = 0
  member this.Gold = 0

let someEnemy = new Enemy("ghost", 10, 1, 3, 0)
