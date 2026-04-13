import scala.io.StdIn.readLine

// text-based User Interface
// Fazer aqui T4, T6, T7 

object TUI {
    
    def showMainMenuPrompt(): Unit = {
        println("\n" + "=" * 25)
        println("=== Welcome to Kōnane ===")
        println("=" * 25)
        print("(n)ew game, (l)oad game, or (q)uit: ")
    }
    
    def showHoleMenuPrompt(): Unit = {
        println("\nChoose where to remove the initial stones:")
        println("1 - Center")
        println("2 - Top Left")
        println("3 - Top Right")
        println("4 - Bottom Left")
        println("5 - Bottom Right")
        print("> ")
    }
    
    def showSizeMenuPrompt(): Unit = {
        println("\nChoose board size:")
        println("4 - 4x4")
        println("6 - 6x6 (Standard)")
        println("8 - 8x8")
        println("10 - 10x10")
        print("> ")
    }
    
    def showValidMoves(moves: List[Coord2D]): Unit = {
        println("\nValid destinations:")
        
        def listMoves(m: List[Coord2D], i: Int): Unit = m match {
            case Nil => ()
            case head :: tail =>
                println(s"$i - $head")
                listMoves(tail, i + 1)
        }
        
        listMoves(moves, 1)
        print("> Choose the move number: ")
    }
    
    def showPlayerTurn(player: Stone): Unit = {
        val name = player match {
            case Stone.Black => "Black (B)"
            case Stone.White => "White (W)"
        }
        println(s"\n>>> Current turn: $name")
    }
    
    def showGameMenuPrompt(): Unit = {
        println("-" * 24)
        println("----- In-Game Menu -----")
        println("-" * 24)
        print("(p)lay, (m)achine random play, (c)heck moves, (r)andom test, or (q)uit: ")
    }
    
    def showCoordinatePrompt(axis: String): Unit = {
        print(s"Enter $axis number: ")
    }
    
    def getUserInput(): String = readLine().trim.toUpperCase
    
    def boardToString(board: Board, size: Int): String = {
        
        // Função recursiva para criar as letras do cabeçalho ("A   B   C...")
        def buildHeader(c: Int): String = {
            if (c >= size) ""
            else {
                val char = (c + 'A').toChar.toString
                if (c == size - 1) char
                else char + "   " + buildHeader(c + 1)
            }
        }
        
        // Função recursiva para criar as pedras de uma única linha ("B | W | . | W")
        def buildCols(l: Int, c: Int): String = {
            if (c >= size) ""
            else {
                val stone = board.get((l, c)) match {
                    case Some(Stone.Black) => "B"
                    case Some(Stone.White) => "W"
                    case None              => "."
                }
                
                if (c == size - 1) stone
                else stone + " | " + buildCols(l, c + 1)
            }
        }
        
        // Função recursiva para criar todas as linhas juntas, separadas por "Enters" (\n)
        def buildRows(l: Int): String = {
            if (l >= size) ""
            else {
                val current = " " + l + " | " + buildCols(l, 0)
                
                if (l == size - 1) current
                else current + "\n" + buildRows(l + 1)
            }
        }
        
        // Montagem Final
        val header = "     " + buildHeader(0)
        val rows = buildRows(0)
        
        header + "\n" + rows
    }
    
    def printBoard(board: Board, size: Int): Unit = {
        val view = boardToString(board, size)
        println(view)
        println()
    }
}
