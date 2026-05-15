import scala.collection.parallel.ParMap

object SaveLoadLogic {

    def saveGameState(board: Board, size: Int, currentPlayer: Stone, timeLimit: Long,
                      openCoords: List[Coord2D], rand: MyRandom, humanColor: Option[Stone],
                      selected: Option[Coord2D], difficulty: Int): String = {

        val playerStr = currentPlayer match {
            case Stone.Black => "B"
            case Stone.White => "W"
        }

        val humanStr = humanColor match {
            case Some(Stone.Black) => "B"
            case Some(Stone.White) => "W"
            case None => "P"
        }

        val openCoordsStr = openCoords.map { case (l, c) => s"$l,$c" }.mkString(";")

        val boardStr = board.toList.map { case ((l, c), stone) =>
            val sStr = if (stone == Stone.Black) "B" else "W"
            s"$l,$c,$sStr"
        }.mkString(";")

        val selectedStr = selected match {
            case Some((l, c)) => s"$l,$c"
            case None => "none"
        }

        s"$size\n$timeLimit\n$playerStr\n${rand.seed}\n$openCoordsStr\n$boardStr\n$humanStr\n$selectedStr\n$difficulty"
    }

    def loadGameState(data: String): Option[(Board, Int, Stone, Long, List[Coord2D], MyRandom, Option[Stone], Option[Coord2D], Int)] = {
        val lines = data.split("\n").toList
        lines match {
            case sizeStr :: timeLimitStr :: playerStr :: seedStr :: openCoordsStr :: boardStr :: humanStr :: selectedStr :: diffStr :: Nil =>
                for {
                    size <- sizeStr.toIntOption
                    timeLimit <- timeLimitStr.toLongOption
                    player <- if (playerStr == "B") Some(Stone.Black) else if (playerStr == "W") Some(Stone.White) else None
                    seed <- seedStr.toLongOption
                    humanColor <- if (humanStr == "B") Some(Some(Stone.Black)) else if (humanStr == "W") Some(Some(Stone.White)) else if (humanStr == "P") Some(None) else None
                    difficulty <- diffStr.toIntOption

                    openCoords = if (openCoordsStr.isEmpty) Nil else openCoordsStr.split(";").toList.flatMap { pair =>
                        pair.split(",") match {
                            case Array(l, c) => Some((l.toInt, c.toInt))
                            case _ => None
                        }
                    }

                    boardList = if (boardStr.isEmpty) Nil else boardStr.split(";").toList.flatMap { triple =>
                        triple.split(",") match {
                            case Array(l, c, "B") => Some(((l.toInt, c.toInt), Stone.Black))
                            case Array(l, c, "W") => Some(((l.toInt, c.toInt), Stone.White))
                            case _ => None
                        }
                    }
                    board = boardList.foldLeft(ParMap[Coord2D, Stone]())((acc, elem) => acc + elem)

                    selected = if (selectedStr == "none") None else {
                        selectedStr.split(",") match {
                            case Array(l, c) => Some((l.toInt, c.toInt))
                            case _ => None
                        }
                    }
                } yield (board, size, player, timeLimit, openCoords, MyRandom(seed), humanColor, selected, difficulty)

            case _ => None
        }
    }
}