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
                
                val (l, c) = coord
                
                // Lista com as 4 direções de salto
                // Formato: (LinhaInimigo, ColunaInimigo, LinhaDestino, ColunaDestino)
                val directions = List(
                    (l - 1, c, l - 2, c), // Salto para Cima
                    (l + 1, c, l + 2, c), // Salto para Baixo
                    (l, c - 1, l, c - 2), // Salto para a Esquerda
                    (l, c + 1, l, c + 2) // Salto para a Direita
                )
                
                def checkDirections(dirs: List[(Int, Int, Int, Int)]): List[Coord2D] = dirs match {
                    case Nil => Nil
                    case (el, ec, dl, dc) :: tail =>
                        // Verifica se a coordenada de destino está dentro do tabuleiro
                        if (dl >= 0 && dl < size && dc >= 0 && dc < size) {
                            // Verifica se está lá o inimigo E se o destino está vazio
                            val hasEnemy = board.get((el, ec)).contains(enemy)
                            val isDestEmpty = board.get((dl, dc)).isEmpty
                            
                            if (hasEnemy && isDestEmpty) {
                                (dl, dc) :: checkDirections(tail)
                            } else {
                                checkDirections(tail)
                            }
                        } else {
                            checkDirections(tail)
                        }
                }
                
                checkDirections(directions)
        }
    }
    
    // T1: Gerar jogada aleatória
    def randomMove(lstOpenCoords: List[Coord2D], rand: MyRandom): (Coord2D, MyRandom) = {
        val (num, nextRandom) = rand.nextInt(lstOpenCoords.length)
        (lstOpenCoords(num), nextRandom)
    }

    // T2: Move a peça e devolve o tabuleiro e a lista das pos válidas
    /*def play(board:Board, player: Stone, coordFrom:Coord2D,coordTo:Coord2D,lstOpenCoords:List[Coord2D]):(Option[Board],List[Coord2D]):(Option[Board], List[Coord2D]) {


    } */
}
