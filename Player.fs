module Player

type Player =
    val mutable hp : int
    val mutable exp : int
    val mutable x : int
    val mutable y : int
    val mutable level : int

    new (hp, exp, x, y, level) = {
        hp = hp
        exp = exp
        x = x
        y = y
        level = level
    }