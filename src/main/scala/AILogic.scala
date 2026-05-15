import scala.collection.parallel.ParMap

object AILogic {
    
    // Função auxiliar que procura TODAS as jogadas possíveis para um jogador
    def getAllPossibleMoves(board: Board, player: Stone, size: Int): List[(Coord2D, Coord2D)] = {
        val playerPieces = board.toList.filter { case (_, stone) => stone == player }.map(_._1)
        playerPieces.flatMap { from =>
            val validDestinations = GameLogic.getValidMovesForPiece(board, from, size)
            validDestinations.map(to => (from, to))
        }
    }
    
    // Função auxiliar para aplicar a jogada escolhida e padronizar o retorno
    private def applyChosenMove(board: Board, player: Stone, from: Coord2D, to: Coord2D, openCoords: List[Coord2D], nextRand: MyRandom): (Option[Board], MyRandom, List[Coord2D], Option[Coord2D]) = {
        val (newBoardOpt, newOpen) = GameLogic.play(board, player, from, to, openCoords)
        (newBoardOpt, nextRand, newOpen, Some(to))
    }
    
    // NÍVEL 1: Fácil (Aleatório)
    def playEasy(board: Board, rand: MyRandom, player: Stone, openCoords: List[Coord2D]): (Option[Board], MyRandom, List[Coord2D], Option[Coord2D]) = {
        GameLogic.playRandomly(board, rand, player, openCoords, GameLogic.randomMove)
    }
    
    // NÍVEL 2: Intermédio (Maior número de capturas)
    def playIntermediate(board: Board, rand: MyRandom, player: Stone, size: Int, openCoords: List[Coord2D]): (Option[Board], MyRandom, List[Coord2D], Option[Coord2D]) = {
        val allMoves = getAllPossibleMoves(board, player, size)
        
        if (allMoves.isEmpty) (None, rand, openCoords, None)
        else {
            val movesWithCaptureCount = allMoves.map { case (from, to) =>
                val capturedList = GameLogic.getCapturedCoords(from, to)
                ((from, to), capturedList.length)
            }
            
            val maxCaptures = movesWithCaptureCount.map(_._2).max
            val bestMoves = movesWithCaptureCount.filter(_._2 == maxCaptures).map(_._1)
            
            val (idx, nextRand) = rand.nextInt(bestMoves.length)
            val (bestFrom, bestTo) = bestMoves(idx)
            
            applyChosenMove(board, player, bestFrom, bestTo, openCoords, nextRand)
        }
    }
    
    // NÍVEL 3: Avançado (Garantir maior mobilidade no turno seguinte)
    def playAdvanced(board: Board, rand: MyRandom, player: Stone, size: Int, openCoords: List[Coord2D]): (Option[Board], MyRandom, List[Coord2D], Option[Coord2D]) = {
        val allMoves = getAllPossibleMoves(board, player, size)
        
        if (allMoves.isEmpty) (None, rand, openCoords, None)
        else {
            val movesWithFutureMobility = allMoves.map { case (from, to) =>
                val capturedList = GameLogic.getCapturedCoords(from, to)
                val simulatedBoard = GameLogic.applyMove(board, from, to, capturedList, player)
                val futureMovesCount = getAllPossibleMoves(simulatedBoard, player, size).length
                ((from, to), futureMovesCount)
            }
            
            val maxMobility = movesWithFutureMobility.map(_._2).max
            val bestMoves = movesWithFutureMobility.filter(_._2 == maxMobility).map(_._1)
            
            val (idx, nextRand) = rand.nextInt(bestMoves.length)
            val (bestFrom, bestTo) = bestMoves(idx)
            
            applyChosenMove(board, player, bestFrom, bestTo, openCoords, nextRand)
        }
    }
}