import scala.annotation.tailrec

object Main extends App {
    
    // Semente inicial para o gerador de números aleatórios
    val initialRandom = MyRandom(7)
    
    menuLoop()
    
    @tailrec
    def menuLoop(): Unit = {
        TUI.showMainMenuPrompt()
        val userInput = TUI.getUserInput()
        
        userInput match {
            case "N" =>
                val size = sizeChoiceLoop()
                val holeChoice = holeChoiceLoop()
                
                val initialBoard = GameLogic.initBoard(size, holeChoice)
                
                val (p1, p2) = holeChoice match {
                    case HolePosition.Center =>
                        val m = size / 2
                        ((m - 1, m - 1), (m - 1, m))
                    case HolePosition.TopLeft => ((0, 0), (0, 1))
                    case HolePosition.TopRight => ((0, size - 1), (0, size - 2))
                    case HolePosition.BottomLeft => ((size - 1, 0), (size - 1, 1))
                    case HolePosition.BottomRight => ((size - 1, size - 1), (size - 1, size - 2))
                }
                
                println(s"\n=== Starting New Game (${size}x${size}) ===")
                
                gameLoop(initialBoard, size, initialRandom, List(p1, p2), Stone.Black)
                
                menuLoop()
            
            case "L" =>
                println("\n[Info] 'Load game' feature is currently under development.")
            
            case "Q" =>
                println("\nExiting Kōnane. Goodbye!")
            
            case _ =>
                println("\n[Error] Invalid option.")
                menuLoop()
        }
    }
    
    @tailrec
    def sizeChoiceLoop(): Int = {
        TUI.showSizeMenuPrompt()
        val input = TUI.getUserInput()
        
        input match {
            case "4" => 4
            case "6" => 6
            case "8" => 8
            case "10" => 10
            case _ =>
                println("\n[Error] Invalid size. Please choose 4, 6, 8, or 10.")
                sizeChoiceLoop()
        }
    }
    
    @tailrec
    def holeChoiceLoop(): HolePosition = {
        TUI.showHoleMenuPrompt()
        val input = TUI.getUserInput()
        
        input match {
            case "1" => HolePosition.Center
            case "2" => HolePosition.TopLeft
            case "3" => HolePosition.TopRight
            case "4" => HolePosition.BottomLeft
            case "5" => HolePosition.BottomRight
            case _ =>
                println("\n[Error] Invalid choice. Please enter a number between 1 and 5.")
                holeChoiceLoop()
        }
    }
    
    @tailrec
    def gameLoop(board: Board, size: Int, rand: MyRandom, openCoords: List[Coord2D], currentPlayer: Stone): Unit = {
        TUI.printBoard(board, size)
        TUI.showPlayerTurn(currentPlayer)
        TUI.showGameMenuPrompt()
        
        val input = TUI.getUserInput()
        input match {
            case "P" => // Play option
                TUI.showCoordinatePrompt("piece row")
                val rFrom = TUI.getUserInput().toIntOption
                TUI.showCoordinatePrompt("piece column")
                val cFrom = TUI.getUserInput().toIntOption
                
                (rFrom, cFrom) match {
                    case (Some(r), Some(c)) =>
                        val from = (r, c)
                        
                        board.get(from) match {
                            case Some(stone) if stone == currentPlayer =>
                                val moves = GameLogic.getValidMovesForPiece(board, from, size)
                                
                                if (moves.isEmpty) {
                                    println("\n[Error] This piece has no valid capture moves!\n")
                                    gameLoop(board, size, rand, openCoords, currentPlayer)
                                } else {
                                    TUI.showValidMoves(moves)
                                    val choice = TUI.getUserInput().toIntOption
                                    
                                    choice match {
                                        case Some(idx) if idx > 0 && idx <= moves.length =>
                                            val to = moves(idx - 1)
                                            val (newBoardOpt, newOpen) = GameLogic.play(board, currentPlayer, from, to, openCoords)
                                            
                                            newBoardOpt match {
                                                case Some(nb) =>
                                                    val nextPlayer = if (currentPlayer == Stone.Black) Stone.White else Stone.Black
                                                    gameLoop(nb, size, rand, newOpen, nextPlayer)
                                                case None =>
                                                    println("\n[Error] The move failed.\n")
                                                    gameLoop(board, size, rand, openCoords, currentPlayer)
                                            }
                                        case _ =>
                                            println("\n[Error] Invalid choice.\n")
                                            gameLoop(board, size, rand, openCoords, currentPlayer)
                                    }
                                }
                            
                            case Some(_) =>
                                println("\n[Error] That is not your piece! Please choose a piece of your color.\n")
                                gameLoop(board, size, rand, openCoords, currentPlayer)
                            
                            case None =>
                                println("\n[Error] There is no piece at that location!\n")
                                gameLoop(board, size, rand, openCoords, currentPlayer)
                        }
                    
                    case _ =>
                        println("\n[Error] Invalid coordinates.\n")
                        gameLoop(board, size, rand, openCoords, currentPlayer)
                }
            
            case "C" =>
                TUI.showCoordinatePrompt("row")
                val rowStr = TUI.getUserInput()
                TUI.showCoordinatePrompt("column")
                val colStr = TUI.getUserInput()
                
                // Usamos toIntOption para evitar o try/catch e manter a recursividade limpa
                (rowStr.toIntOption, colStr.toIntOption) match {
                    case (Some(row), Some(col)) =>
                        val moves = GameLogic.getValidMovesForPiece(board, (row, col), size)
                        println(s"\n[Result] Valid moves for the piece at ($row, $col): $moves\n")
                        gameLoop(board, size, rand, openCoords, currentPlayer)
                    
                    case _ =>
                        // Se qualquer um deles for None (letras vazias, etc.)
                        println("\n[Error] Invalid coordinates. Please enter integer numbers.\n")
                        gameLoop(board, size, rand, openCoords, currentPlayer)
                }
            
            case "R" =>
                TUI.showCoordinatePrompt("row")
                val rowStr = TUI.getUserInput()
                TUI.showCoordinatePrompt("column")
                val colStr = TUI.getUserInput()
                
                // Usamos toIntOption para evitar o try/catch e manter a recursividade limpa
                (rowStr.toIntOption, colStr.toIntOption) match {
                    case (Some(row), Some(col)) =>
                        val coord = (row, col)
                        val moves = GameLogic.getValidMovesForPiece(board, coord, size)
                        
                        if (moves.isEmpty) {
                            println(s"\n[Result] T1 Test - No valid moves for the piece at $coord!\n")
                            gameLoop(board, size, rand, openCoords, currentPlayer)
                        } else {
                            val (chosenDest, nextRandom) = GameLogic.randomMove(moves, rand)
                            
                            println(s"\n[Result] T1 Test - Valid destinations for $coord: $moves")
                            println(s"[Result] T1 Test - Randomly chosen destination: $chosenDest\n")
                            
                            gameLoop(board, size, nextRandom, openCoords, currentPlayer)
                        }
                    
                    case _ =>
                        println("\n[Error] Invalid coordinates. Please enter integer numbers.\n")
                        gameLoop(board, size, rand, openCoords, currentPlayer)
                }
            
            case "Q" =>
                println("\nReturning to Main Menu...")
            
            case _ =>
                println("\n[Error] Invalid option.\n")
                gameLoop(board, size, rand, openCoords, currentPlayer)
        }
    }
}