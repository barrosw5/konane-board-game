import scala.annotation.tailrec

object Main extends App {
    
    // Semente inicial para o gerador de números aleatórios
    val initialRandom = MyRandom(7)
    
    menuLoop()


    // Loop do menu inicial
    @tailrec
    def menuLoop(): Unit = {
        TUI.showMainMenuPrompt()
        val userInput = TUI.getUserInput()
        
        userInput match {
            case "N" =>
                val size = sizeChoiceLoop()
                val holeChoice = holeChoiceLoop()
                val timeLimit = timeLimitChoiceLoop()
                
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
                
                gameLoop(initialBoard, size, initialRandom, List(p1, p2), Stone.Black, timeLimit, Nil)
                
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

    // Escolha dentro do menu
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
    def timeLimitChoiceLoop(): Long = {
        println("\nChoose how much time you got for each play (in seconds):")
        print("> ")
        val input = TUI.getUserInput()
        input.toLongOption match {
            case Some(s) if s > 0 => s * 1000 // Convertemos para milisegundos
            case _ =>
                println("\n[Error] Invalid value. Please enter a number greater than 0.")
                timeLimitChoiceLoop()
        }
    }


    // Loop do jogo em si
    @tailrec
    def gameLoop(board: Board, size: Int, rand: MyRandom, openCoords: List[Coord2D], currentPlayer: Stone, timeLimit: Long, lstBoardsHistory: List[(Board, List[Coord2D], Stone, MyRandom)]): Unit = {
        TUI.printBoard(board, size)
        
        // Verificação da T5
        if (!GameLogic.hasValidMoves(board, currentPlayer, size)) {
            val winner = if (currentPlayer == Stone.Black) "White" else "Black"
            println(s"\n[Game Over] $currentPlayer has no moves left. $winner wins!")
            return // Sai do loop e volta ao menu
        }
        
        TUI.showPlayerTurn(currentPlayer)
        
        val startTime = System.currentTimeMillis()
        
        TUI.showGameMenuPrompt()
        
        val input = TUI.getUserInput()
        
        val endTime = System.currentTimeMillis()
        val elapsed = endTime - startTime
        
        if (elapsed > timeLimit) {
            println(s"\n[Timeout] You took ${elapsed / 1000.0}s! The time limit was ${timeLimit / 1000.0}s.")
            val winner = if (currentPlayer == Stone.Black) "White" else "Black"
            println(s"$winner wins!")
        }
        else {
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
                                        gameLoop(board, size, rand, openCoords, currentPlayer, timeLimit, lstBoardsHistory)
                                    } else {
                                        TUI.showValidMoves(moves)
                                        val choice = TUI.getUserInput().toIntOption

                                        choice match {
                                            case Some(idx) if idx > 0 && idx <= moves.length =>
                                                val to = moves(idx - 1)
                                                val newHistory = storeBoard(lstBoardsHistory, board, openCoords, currentPlayer, rand)
                                                val (newBoardOpt, newOpen) = GameLogic.play(board, currentPlayer, from, to, openCoords)

                                                newBoardOpt match {
                                                    case Some(nb) =>
                                                        val nextPlayer = if (currentPlayer == Stone.Black) Stone.White else Stone.Black
                                                        gameLoop(nb, size, rand, newOpen, nextPlayer, timeLimit, newHistory)
                                                    case None =>
                                                        println("\n[Error] The move failed.\n")
                                                        gameLoop(board, size, rand, openCoords, currentPlayer, timeLimit, lstBoardsHistory)
                                                }
                                            case _ =>
                                                println("\n[Error] Invalid choice.\n")
                                                gameLoop(board, size, rand, openCoords, currentPlayer, timeLimit, lstBoardsHistory)
                                        }
                                    }
                                
                                case Some(_) =>
                                    println("\n[Error] That is not your piece! Please choose a piece of your color.\n")
                                    gameLoop(board, size, rand, openCoords, currentPlayer, timeLimit, lstBoardsHistory)

                                case None =>
                                    println("\n[Error] There is no piece at that location!\n")
                                    gameLoop(board, size, rand, openCoords, currentPlayer, timeLimit, lstBoardsHistory)
                            }

                        case _ =>
                            println("\n[Error] Invalid coordinates.\n")
                            gameLoop(board, size, rand, openCoords, currentPlayer, timeLimit, lstBoardsHistory)
                    }
                
                case "M" => // Machine play
                    val newHistory = storeBoard(lstBoardsHistory, board, openCoords, currentPlayer, rand)
                    val (newBoardOpt, nextRand, newOpen, toOpt) = GameLogic.playRandomly(board, rand, currentPlayer, openCoords, GameLogic.randomMove)

                    newBoardOpt match {
                        case Some(nb) =>
                            println(s"\n[Result] The computer randomly moved a piece to ${toOpt.get}!\n")
                            val nextPlayer = if (currentPlayer == Stone.Black) Stone.White else Stone.Black
                            gameLoop(nb, size, nextRand, newOpen, nextPlayer, timeLimit, newHistory)

                        case None =>
                            println("\n[Result] This player has no valid moves left. Game Over!\n")
                            gameLoop(board, size, nextRand, openCoords, currentPlayer, timeLimit, lstBoardsHistory)
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
                            gameLoop(board, size, rand, openCoords, currentPlayer, timeLimit, lstBoardsHistory)

                        case _ =>
                            println("\n[Error] Invalid coordinates. Please enter integer numbers.\n")
                            gameLoop(board, size, rand, openCoords, currentPlayer, timeLimit, lstBoardsHistory)
                    }
                case "U" =>
                    TUI.showTextPrompt("Tem a certeza (0) SIM ou (1) NÃO: ")
                    val choice = TUI.getUserInput()

                    choice match {
                        case "0" =>
                            if ( lstBoardsHistory.isEmpty) {
                                println("\n[Error] No moves to undo.\n")
                                gameLoop(board, size, rand, openCoords, currentPlayer, timeLimit, lstBoardsHistory)
                            }
                            else {
                                val (boardPrev, openCoordsPrev, currentPlayerPrev, randPrev) = lstBoardsHistory.head
                                gameLoop(boardPrev, size, randPrev, openCoordsPrev, currentPlayerPrev, timeLimit, lstBoardsHistory.tail)
                            }
                        case "1" =>
                            gameLoop(board, size, rand, openCoords, currentPlayer, timeLimit, lstBoardsHistory)
                        case _ =>
                            println("\n[Error] Invalid option.\n")
                            gameLoop(board, size, rand, openCoords, currentPlayer, timeLimit, lstBoardsHistory)
                    }


                case "Q" =>
                    println("\nReturning to Main Menu...")

                case _ =>
                    println("\n[Error] Invalid option.\n")
                    gameLoop(board, size, rand, openCoords, currentPlayer, timeLimit, lstBoardsHistory)
            }
        }
    }
}