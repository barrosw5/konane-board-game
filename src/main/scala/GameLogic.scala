import scala.annotation.tailrec
import scala.collection.parallel.ParMap

// Fazer aqui T1, T2, T3, T5

object GameLogic {
    
    // inicializa o board
    def initBoard(size: Int, hole: HolePosition): Board = {
        
        def buildMap(l: Int, c: Int, acc: Board): Board = {
            if (l >= size) acc
            else if (c >= size) buildMap(l + 1, 0, acc)
            else {
                val stone = if ((l + c) % 2 == 0) Stone.Black else Stone.White
                val newAcc = acc + ((l, c) -> stone)
                buildMap(l, c + 1, newAcc)
            }
        }
        
        val initMap: Board = buildMap(0, 0, ParMap())
        
        // Decide as peças a remover consoante o menu
        val (p1, p2) = hole match {
            case HolePosition.Center =>
                val middle = size / 2
                ((middle - 1, middle - 1), (middle - 1, middle))
            case HolePosition.TopLeft => ((0, 0), (0, 1))
            case HolePosition.TopRight => ((0, size - 1), (0, size - 2))
            case HolePosition.BottomLeft => ((size - 1, 0), (size - 1, 1))
            case HolePosition.BottomRight => ((size - 1, size - 1), (size - 1, size - 2))
        }
        
        initMap - p1 - p2
    }
    
    // Função que verifica as 4 direções possíveis para a peça numa dada coordenada
    def getValidMovesForPiece(board: Board, coord: Coord2D, size: Int): List[Coord2D] = {
        board.get(coord) match {
            case None => Nil
            case Some(myStone) =>
                val enemy = myStone match {
                    case Stone.Black => Stone.White
                    case Stone.White => Stone.Black
                }
                
                // direção do movimento: (deltaLinha, deltaColuna)
                val directions = List((-1, 0), (1, 0), (0, -1), (0, 1))
                
                // Função recursiva interna para explorar vários saltos
                def exploreDirection(currL: Int, currC: Int, dL: Int, dC: Int): List[Coord2D] = {
                    val enemyL = currL + dL
                    val enemyC = currC + dC
                    val destL = currL + 2 * dL
                    val destC = currC + 2 * dC
                    
                    // Verifica se o destino ainda está dentro dos limites do tabuleiro
                    if (destL >= 0 && destL < size && destC >= 0 && destC < size) {
                        val hasEnemy = board.get((enemyL, enemyC)).contains(enemy)
                        val isDestEmpty = board.get((destL, destC)).isEmpty
                        
                        if (hasEnemy && isDestEmpty) {
                            val validDest = (destL, destC)
                            validDest :: exploreDirection(destL, destC, dL, dC)
                        } else {
                            Nil
                        }
                    } else {
                        Nil
                    }
                }
                
                // Avalia as 4 direções recursivamente e junta todas as listas válidas (:::)
                def checkDirections(dirs: List[(Int, Int)]): List[Coord2D] = dirs match {
                    case Nil => Nil
                    case (dL, dC) :: tail =>
                        exploreDirection(coord._1, coord._2, dL, dC) ++ checkDirections(tail)
                }
                
                checkDirections(directions)
        }
    }
    
    // T1: Gerar jogada aleatória
    def randomMove(lstOpenCoords: List[Coord2D], rand: MyRandom): (Coord2D, MyRandom) = {
        val (num, nextRandom) = rand.nextInt(lstOpenCoords.length)
        (lstOpenCoords(num), nextRandom)
    }


    def getSize(board: Board, lstOpenCoords: List[Coord2D]): Int = {
        // Retorna o tamanho do tabuleiro. No fundo dá o tamanho do board (casas preenchidas) + as casas livres, na lista lstOpenCoords
        val total = board.size + lstOpenCoords.size
        math.sqrt(total).toInt
    }

    def isValidPlay(board: Board, player: Stone, from: Coord2D, to: Coord2D, size: Int): Boolean = {
        board.get(from) match {
            case Some(stone) if stone == player =>
                val validMoves = getValidMovesForPiece(board, from, size)
                validMoves.contains(to)
            case _ =>
                false
        }
    }
    
    // Recolhe todas as coordenadas capturadas entre a origem e o destino final
    def getCapturedCoords(from: Coord2D, to: Coord2D): List[Coord2D] = {
        val (fromRow, fromCol) = from
        val (toRow, toCol) = to
        
        // Determina a direção matemática do salto (1, -1 ou 0)
        val dL = if (toRow > fromRow) 1 else if (toRow < fromRow) -1 else 0
        val dC = if (toCol > fromCol) 1 else if (toCol < fromCol) -1 else 0
        
        @tailrec
        def collect(currL: Int, currC: Int, acc: List[Coord2D]): List[Coord2D] = {
            if (currL == toRow && currC == toCol) acc // Chegou ao fim do salto
            else {
                val captured = (currL + dL, currC + dC)
                // Avança duas casas de cada vez no ciclo
                collect(currL + dL * 2, currC + dC * 2, captured :: acc)
            }
        }
        
        collect(fromRow, fromCol, Nil)
    }

    /*def updateOpenCoord(lstOpenCoords: List[Coord2D], from: Coord2D, to: Coord2D, captured: Coord2D): List[Coord2D] = {
       // Atualiza a lista das pos vazias. Retira a de destino e acrescenta a de origem e capturada
        val withOutTo = lstOpenCoords.filter (_ != to)
        from :: captured :: withOutTo
    }*/
    
    def updateOpenCoord(lstOpenCoords: List[Coord2D], from: Coord2D, to: Coord2D, capturedList: List[Coord2D]): List[Coord2D] = {
        val withOutTo = lstOpenCoords.filter(_ != to)
        // O operador ::: concatena a lista das peças capturadas com as posições livres
        from :: (capturedList ++ withOutTo)
    }

    /*def applyMove(board: Board, from: Coord2D, to: Coord2D, captured: Coord2D, player: Stone): Board = {
        // Remove do Map e ao remover a chave do map está a colocar a Coordenada no tabuleiro vazia
        val boardWithOutFrom = board - from
        val boardWithOutCaptured = boardWithOutFrom - captured
        // to -> player basicamente cria um par-chave para a Stone
        boardWithOutCaptured + ( to -> player)
    }*/
    
    def applyMove(board: Board, from: Coord2D, to: Coord2D, capturedList: List[Coord2D], player: Stone): Board = {
        // Retira a origem, e o foldLeft trata de retirar todas as coordenadas da lista capturada do Board
        val boardWithoutCaptured = (capturedList foldLeft (board - from)) ((accBoard, capCoord) => accBoard - capCoord)
        boardWithoutCaptured + (to -> player)
    }

    // T2: Move a peça e devolve o tabuleiro e a lista das pos válidas
    /*def play(board:Board, player: Stone, coordFrom:Coord2D,coordTo:Coord2D,lstOpenCoords:List[Coord2D]):(Option[Board], List[Coord2D]) = {
        val size = getSize(board, lstOpenCoords)
        if (!isValidPlay(board, player, coordFrom, coordTo, size)) {
            (None, lstOpenCoords)
        }
        else {
            val captured = middleCoord(coordFrom, coordTo)
            val newBoard = applyMove(board, coordFrom, coordTo, captured, player)
            val newOpenLs = updateOpenCoord(lstOpenCoords, coordFrom, coordTo, captured)

            (Some(newBoard), newOpenLs)
        }
    }*/
    
    def play(board: Board, player: Stone, coordFrom: Coord2D, coordTo: Coord2D, lstOpenCoords: List[Coord2D]): (Option[Board], List[Coord2D]) = {
        val size = getSize(board, lstOpenCoords)
        if (!isValidPlay(board, player, coordFrom, coordTo, size)) {
            (None, lstOpenCoords)
        } else {
            // Agora obtemos uma lista em vez de uma única opção
            val capturedList = getCapturedCoords(coordFrom, coordTo)
            val newBoard = applyMove(board, coordFrom, coordTo, capturedList, player)
            val newOpenLs = updateOpenCoord(lstOpenCoords, coordFrom, coordTo, capturedList)
            
            (Some(newBoard), newOpenLs)
        }
    }
    
    
    
    // T3: Realizar uma jogada aleatória
    /*def playRandomly(board:Board,
                    r:MyRandom,
                    player:Stone,
                    lstOpenCoords:List[Coord2D],
                    f:(List[Coord2D],MyRandom)=>(Coord2D,MyRandom)):
                    (Option[Board],MyRandom,List[Coord2D],Option[Coord2D]) = {
        val (coord, nextR) = f(lstOpenCoords,MyRandom)
        play(coord)
    }*/

}
