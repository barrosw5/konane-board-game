import scala.collection.parallel.ParMap
import scala.annotation.tailrec

object SaveLoadLogic {

    def saveGameState(board: Board, size: Int, currentPlayer: Stone, timeLimit: Long,
                      openCoords: List[Coord2D], rand: MyRandom, humanColor: Option[Stone],
                      selected: Option[Coord2D], difficulty: Int,
                      history: List[(Board, List[Coord2D], Stone, MyRandom)], hole: HolePosition): String = {

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
            s"$l,$c,${if (stone == Stone.Black) "B" else "W"}"
        }.mkString(";")

        val selectedStr = selected match {
            case Some((l, c)) => s"$l,$c"
            case None => "none"
        }

        def serializeBoard(b: Board): String =
            b.toList.map { case ((l, c), stone) =>
                s"$l,$c,${if (stone == Stone.Black) "B" else "W"}"
            }.mkString(";")

        val historyStr = history.map { case (b, oc, p, r) =>
            s"${serializeBoard(b)}|${oc.map { case (l, c) => s"$l,$c" }.mkString(";")}|${if (p == Stone.Black) "B" else "W"}|${r.seed}"
        }.mkString(";;")

        val holeStr = hole match {
            case HolePosition.Center => "Center"
            case HolePosition.TopLeft => "TopLeft"
            case HolePosition.TopRight => "TopRight"
            case HolePosition.BottomLeft => "BottomLeft"
            case HolePosition.BottomRight => "BottomRight"
        }

        s"$size\n$timeLimit\n$playerStr\n${rand.seed}\n$openCoordsStr\n$boardStr\n$humanStr\n$selectedStr\n$difficulty\n$historyStr\n$holeStr"
    }

    private def parseBoard(str: String): Option[Board] = {
        if (str.isEmpty) Some(ParMap.empty)
        else {
            @tailrec
            def build(acc: Board, tokens: List[String]): Option[Board] = tokens match {
                case Nil => Some(acc)
                case t :: rest =>
                    t.split(",") match {
                        case Array(l, c, "B") => build(acc + ((l.toInt, c.toInt) -> Stone.Black), rest)
                        case Array(l, c, "W") => build(acc + ((l.toInt, c.toInt) -> Stone.White), rest)
                        case _ => None
                    }
            }
            build(ParMap.empty, str.split(";").toList)
        }
    }

    private def parseOpenCoords(str: String): List[Coord2D] = {
        if (str.isEmpty) Nil
        else str.split(";").toList.flatMap { pair =>
            pair.split(",") match {
                case Array(l, c) => Some((l.toInt, c.toInt))
                case _ => None
            }
        }
    }

    private def parseHistory(historyStr: String): Option[List[(Board, List[Coord2D], Stone, MyRandom)]] = {
        if (historyStr.isEmpty) Some(Nil)
        else {
            @tailrec
            def parseStates(remaining: List[String], acc: List[(Board, List[Coord2D], Stone, MyRandom)]): Option[List[(Board, List[Coord2D], Stone, MyRandom)]] = {
                remaining match {
                    case Nil => Some(acc.reverse)
                    case state :: rest =>
                        state.split('|') match {
                            case Array(bStr, ocStr, pStr, seedStr) =>
                                parseBoard(bStr) match {
                                    case Some(board) =>
                                        val openCoords = parseOpenCoords(ocStr)
                                        val player = if (pStr == "B") Stone.Black else Stone.White
                                        seedStr.toLongOption match {
                                            case Some(seed) => parseStates(rest, (board, openCoords, player, MyRandom(seed)) :: acc)
                                            case None => None
                                        }
                                    case None => None
                                }
                            case _ => None
                        }
                }
            }

            parseStates(historyStr.split(";;").toList, Nil)
        }
    }

    def loadGameState(data: String): Option[(Board, Int, Stone, Long, List[Coord2D], MyRandom, Option[Stone], Option[Coord2D], Int, List[(Board, List[Coord2D], Stone, MyRandom)], HolePosition)] = {
        data.split("\n").toList match {
            case sizeStr :: timeLimitStr :: playerStr :: seedStr :: openCoordsStr :: boardStr :: humanStr :: selectedStr :: diffStr :: historyStr :: holeStr :: Nil =>
                sizeStr.toIntOption.flatMap { size =>
                    timeLimitStr.toLongOption.flatMap { timeLimit =>
                        val playerOpt = if (playerStr == "B") Some(Stone.Black) else if (playerStr == "W") Some(Stone.White) else None
                        playerOpt.flatMap { player =>
                            seedStr.toLongOption.flatMap { seed =>
                                val humanColorOpt = if (humanStr == "B") Some(Some(Stone.Black)) else if (humanStr == "W") Some(Some(Stone.White)) else if (humanStr == "P") Some(None) else None
                                humanColorOpt.flatMap { humanColor =>
                                    diffStr.toIntOption.flatMap { diff =>
                                        parseBoard(boardStr).flatMap { board =>
                                            val openCoords = parseOpenCoords(openCoordsStr)
                                            val selected = if (selectedStr == "none") None else {
                                                selectedStr.split(",") match {
                                                    case Array(l, c) => Some((l.toInt, c.toInt))
                                                    case _ => None
                                                }
                                            }
                                            parseHistory(historyStr).map { history =>
                                                val hole = holeStr match {
                                                    case "Center" => HolePosition.Center
                                                    case "TopLeft" => HolePosition.TopLeft
                                                    case "TopRight" => HolePosition.TopRight
                                                    case "BottomLeft" => HolePosition.BottomLeft
                                                    case "BottomRight" => HolePosition.BottomRight
                                                    case _ => HolePosition.Center
                                                }
                                                (board, size, player, timeLimit, openCoords, MyRandom(seed), humanColor, selected, diff, history, hole)
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            case _ => None
        }
    }
}