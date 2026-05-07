import scala.collection.parallel.ParMap

object SaveLoadLogic {
    
    // GRAVAR (Estado -> String)
    def saveGameState(board: Board, size: Int, currentPlayer: Stone, timeLimit: Long, openCoords: List[Coord2D], rand: MyRandom): String = {
        
        val playerStr = currentPlayer match {
            case Stone.Black => "B"
            case Stone.White => "W"
        }
        
        // "linha,coluna;linha,coluna"
        val openCoordsStr = openCoords.map { case (l, c) => s"$l,$c" }.mkString(";")
        
        // "linha,coluna,Pedra;linha,coluna,Pedra"
        val boardStr = board.toList.map { case ((l, c), stone) =>
            val sStr = if (stone == Stone.Black) "B" else "W"
            s"$l,$c,$sStr"
        }.mkString(";")
        
        // Juntamos tudo com quebras de linha
        s"$size\n$timeLimit\n$playerStr\n${rand.seed}\n$openCoordsStr\n$boardStr"
    }
    
    // CARREGAR (String -> Option[Estado]) - FUNÇÃO PURA
    def loadGameState(data: String): Option[(Board, Int, Stone, Long, List[Coord2D], MyRandom)] = {
        val lines = data.split("\n")
        
        // Pattern matching para garantir que temos as 6 linhas exatas
        lines.toList match {
            case sizeStr :: timeLimitStr :: playerStr :: seedStr :: openCoordsStr :: boardStr :: Nil =>
                for {
                    size <- sizeStr.toIntOption
                    timeLimit <- timeLimitStr.toLongOption
                    player <- if (playerStr == "B") Some(Stone.Black) else if (playerStr == "W") Some(Stone.White) else None
                    seed <- seedStr.toLongOption
                    
                    // Tratamento das coordenadas livres
                    openCoords = if (openCoordsStr.isEmpty) Nil else openCoordsStr.split(";").toList.flatMap { pair =>
                        pair.split(",") match {
                            case Array(l, c) => Some((l.toInt, c.toInt))
                            case _ => None
                        }
                    }
                    
                    // Tratamento do tabuleiro (ParMap reconstruído com foldLeft para ser puramente funcional)
                    boardList = if (boardStr.isEmpty) Nil else boardStr.split(";").toList.flatMap { triple =>
                        triple.split(",") match {
                            case Array(l, c, "B") => Some(((l.toInt, c.toInt), Stone.Black))
                            case Array(l, c, "W") => Some(((l.toInt, c.toInt), Stone.White))
                            case _ => None
                        }
                    }
                    board = boardList.foldLeft(ParMap[Coord2D, Stone]())((acc, elem) => acc + elem)
                    
                } yield (board, size, player, timeLimit, openCoords, MyRandom(seed))
            
            case _ => None // O ficheiro não tem a estrutura correta
        }
    }
}