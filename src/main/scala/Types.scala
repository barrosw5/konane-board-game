import scala.collection.parallel.ParMap

type Coord2D = (Int, Int) // (row, column)

type Board = ParMap[Coord2D, Stone]

// Enum com as 5 posições possíveis
enum HolePosition:
    case Center, TopLeft, TopRight, BottomLeft, BottomRight

enum Stone:
    case Black, White