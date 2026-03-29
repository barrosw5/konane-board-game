import scala.collection.parallel.ParMap

// Fazer aqui T1, T2, T3, T5

object GameLogic {
    
    // inicializa o board sem as duas pedras do meio
    def initBoard(size: Int): Board = {
        
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
        
        val middle = size / 2
        val openMap = initMap - ((middle - 1, middle - 1)) - ((middle - 1, middle))
        
        openMap.par
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
