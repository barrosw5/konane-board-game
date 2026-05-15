import scala.annotation.tailrec

object Main extends App {

    val initialRandom = saveRandom.loadRandomFromFile("seed.txt")
    menuLoop(initialRandom)

    @tailrec
    def menuLoop(currentRand: MyRandom): Unit = {
        TUI.showMainMenuPrompt()
        val userInput = TUI.getUserInput()

        userInput match {
            case "N" =>
                val humanColor = colorChoiceLoop()
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
<<<<<<< Updated upstream
                
                val finalRand = gameLoop(initialBoard, size, currentRand, List(p1, p2), Stone.Black, timeLimit, Nil, humanColor)
                
=======

                val finalRand = gameLoop(initialBoard, size, currentRand, List(p1, p2), Stone.Black, timeLimit, Nil, humanColor, difficulty)

>>>>>>> Stashed changes
                menuLoop(finalRand)

            case "L" =>
                val fileContentOpt: Option[String] = try {
                    val source = scala.io.Source.fromFile("savegame.txt")
                    val content = source.mkString
                    source.close()
                    Some(content)
                } catch {
                    case _: java.io.FileNotFoundException =>
                        println("\n[Error] No save game found ('savegame.txt' is missing).")
                        None
                    case e: Exception =>
                        println(s"\n[Error] Could not read file: ${e.getMessage}")
                        None
                }

                fileContentOpt match {
                    case Some(fileContent) =>
                        SaveLoadLogic.loadGameState(fileContent) match {
                            case Some((loadedBoard, loadedSize, loadedPlayer, loadedLimit, loadedOpenCoords, loadedRand, loadedHumanColor,_,_)) =>
                                println("\n=== Game Loaded Successfully! ===")
                                val finalRand = gameLoop(loadedBoard, loadedSize, loadedRand, loadedOpenCoords, loadedPlayer, loadedLimit, Nil, loadedHumanColor)
                                menuLoop(finalRand)
                            case None =>
                                println("\n[Error] The save file is corrupted or in an invalid format.")
                                menuLoop(currentRand)
                        }
                    case None =>
                        menuLoop(currentRand)
                }

            case "Q" =>
                saveRandom.saveRandomToFile("seed.txt", currentRand)
                println("\nExiting Kōnane. Goodbye!")

            case _ =>
                println("\n[Error] Invalid option.")
                menuLoop(currentRand)
        }
    }

    @tailrec
    def colorChoiceLoop(): Option[Stone] = {
        TUI.showColorMenuPrompt()
        val input = TUI.getUserInput()

        input match {
            case "1" => Some(Stone.Black)
            case "2" => Some(Stone.White)
            case "3" => None
            case _ =>
                println("\n[Error] Invalid choice. Please enter 1, 2, or 3.")
                colorChoiceLoop()
        }
    }

    @tailrec
<<<<<<< Updated upstream
=======
    def difficultyChoiceLoop(): Int = {
        TUI.showDifficultyMenuPrompt()
        val input = TUI.getUserInput()
        input match {
            case "1" => 1
            case "2" => 2
            case "3" => 3
            case _ =>
                println("\n[Error] Invalid choice. Please choose 1, 2, or 3.")
                difficultyChoiceLoop()
        }
    }

    @tailrec
>>>>>>> Stashed changes
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
            case Some(s) if s > 0 => s * 1000
            case _ =>
                println("\n[Error] Invalid value. Please enter a number greater than 0.")
                timeLimitChoiceLoop()
        }
    }

    @tailrec
    def gameLoop(board: Board, size: Int, rand: MyRandom, openCoords: List[Coord2D], currentPlayer: Stone, timeLimit: Long, lstBoardsHistory: List[(Board, List[Coord2D], Stone, MyRandom)], humanColor: Option[Stone]): MyRandom = {
        TUI.printBoard(board, size)

        if (!GameLogic.hasValidMoves(board, currentPlayer, size)) {
            val winner = if (currentPlayer == Stone.Black) "White" else "Black"
            println(s"\n[Game Over] $currentPlayer has no moves left. $winner wins!")
            rand
        } else {
            TUI.showPlayerTurn(currentPlayer)

            // Verifica se é a vez da máquina
            val isMachineTurn = humanColor match {
                case Some(color) if color != currentPlayer => true
                case _ => false
            }

            if (isMachineTurn) {
                println("\n>>> The machine is thinking...")
                Thread.sleep(1500) // Pausa para melhorar a UX na consola

                val newHistory = GameLogic.storeBoard(lstBoardsHistory, board, openCoords, currentPlayer, rand)
<<<<<<< Updated upstream
                val (newBoardOpt, nextRand, newOpen, toOpt) = GameLogic.playRandomly(board, rand, currentPlayer, openCoords, GameLogic.randomMove)
                
=======

                // Escolhe a função de AI consoante a dificuldade
                val (newBoardOpt, nextRand, newOpen, toOpt) = difficulty match {
                    case 1 => AILogic.playEasy(board, rand, currentPlayer, openCoords)
                    case 2 => AILogic.playIntermediate(board, rand, currentPlayer, size, openCoords)
                    case 3 => AILogic.playAdvanced(board, rand, currentPlayer, size, openCoords)
                }

>>>>>>> Stashed changes
                newBoardOpt match {
                    case Some(nb) =>
                        println(s"\n[Result] The computer randomly moved a piece to ${toOpt.get}!\n")
                        val nextPlayer = if (currentPlayer == Stone.Black) Stone.White else Stone.Black
<<<<<<< Updated upstream
                        gameLoop(nb, size, nextRand, newOpen, nextPlayer, timeLimit, newHistory, humanColor)
                    
=======
                        gameLoop(nb, size, nextRand, newOpen, nextPlayer, timeLimit, newHistory, humanColor, difficulty)

>>>>>>> Stashed changes
                    case None =>
                        println("\n[Result] This player has no valid moves left. Game Over!\n")
                        gameLoop(board, size, nextRand, openCoords, currentPlayer, timeLimit, lstBoardsHistory, humanColor)
                }
            } else {
                val startTime = System.currentTimeMillis()
                TUI.showGameMenuPrompt()
                val input = TUI.getUserInput()
                val endTime = System.currentTimeMillis()
                val elapsed = endTime - startTime

                if (elapsed > timeLimit) {
                    println(s"\n[Timeout] You took ${elapsed / 1000.0}s! The time limit was ${timeLimit / 1000.0}s.")
                    val winner = if (currentPlayer == Stone.Black) "White" else "Black"
                    println(s"$winner wins!")
                    rand
                }
                else {
                    input match {
                        case "P" =>
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
                                                gameLoop(board, size, rand, openCoords, currentPlayer, timeLimit, lstBoardsHistory, humanColor)
                                            } else {
                                                TUI.showValidMoves(moves)
                                                val choice = TUI.getUserInput().toIntOption
                                                choice match {
                                                    case Some(idx) if idx > 0 && idx <= moves.length =>
                                                        val to = moves(idx - 1)
                                                        val newHistory = GameLogic.storeBoard(lstBoardsHistory, board, openCoords, currentPlayer, rand)
                                                        val (newBoardOpt, newOpen) = GameLogic.play(board, currentPlayer, from, to, openCoords)

                                                        newBoardOpt match {
                                                            case Some(nb) =>
                                                                val nextPlayer = if (currentPlayer == Stone.Black) Stone.White else Stone.Black
                                                                gameLoop(nb, size, rand, newOpen, nextPlayer, timeLimit, newHistory, humanColor)
                                                            case None =>
                                                                println("\n[Error] The move failed.\n")
                                                                gameLoop(board, size, rand, openCoords, currentPlayer, timeLimit, lstBoardsHistory, humanColor)
                                                        }
                                                    case _ =>
                                                        println("\n[Error] Invalid choice.\n")
                                                        gameLoop(board, size, rand, openCoords, currentPlayer, timeLimit, lstBoardsHistory, humanColor)
                                                }
                                            }
                                        case Some(_) =>
                                            println("\n[Error] That is not your piece! Please choose a piece of your color.\n")
                                            gameLoop(board, size, rand, openCoords, currentPlayer, timeLimit, lstBoardsHistory, humanColor)
                                        case None =>
                                            println("\n[Error] There is no piece at that location!\n")
                                            gameLoop(board, size, rand, openCoords, currentPlayer, timeLimit, lstBoardsHistory, humanColor)
                                    }
                                case _ =>
                                    println("\n[Error] Invalid coordinates.\n")
                                    gameLoop(board, size, rand, openCoords, currentPlayer, timeLimit, lstBoardsHistory, humanColor)
                            }

                        case "M" =>
                            // Mantido como "batota" ou dica para o jogador humano pedir à máquina para jogar por ele
                            val newHistory = GameLogic.storeBoard(lstBoardsHistory, board, openCoords, currentPlayer, rand)
<<<<<<< Updated upstream
                            val (newBoardOpt, nextRand, newOpen, toOpt) = GameLogic.playRandomly(board, rand, currentPlayer, openCoords, GameLogic.randomMove)
                            
=======

                            val (newBoardOpt, nextRand, newOpen, toOpt) = difficulty match {
                                case 1 => AILogic.playEasy(board, rand, currentPlayer, openCoords)
                                case 2 => AILogic.playIntermediate(board, rand, currentPlayer, size, openCoords)
                                case 3 => AILogic.playAdvanced(board, rand, currentPlayer, size, openCoords)
                            }

>>>>>>> Stashed changes
                            newBoardOpt match {
                                case Some(nb) =>
                                    println(s"\n[Result] The computer randomly moved a piece to ${toOpt.get}!\n")
                                    val nextPlayer = if (currentPlayer == Stone.Black) Stone.White else Stone.Black
                                    gameLoop(nb, size, nextRand, newOpen, nextPlayer, timeLimit, newHistory, humanColor)
                                case None =>
                                    println("\n[Result] This player has no valid moves left. Game Over!\n")
                                    gameLoop(board, size, nextRand, openCoords, currentPlayer, timeLimit, lstBoardsHistory, humanColor)
                            }

                        case "C" =>
                            TUI.showCoordinatePrompt("row")
                            val rowStr = TUI.getUserInput()
                            TUI.showCoordinatePrompt("column")
                            val colStr = TUI.getUserInput()
                            (rowStr.toIntOption, colStr.toIntOption) match {
                                case (Some(row), Some(col)) =>
                                    val moves = GameLogic.getValidMovesForPiece(board, (row, col), size)
                                    println(s"\n[Result] Valid moves for the piece at ($row, $col): $moves\n")
                                    gameLoop(board, size, rand, openCoords, currentPlayer, timeLimit, lstBoardsHistory, humanColor)
                                case _ =>
                                    println("\n[Error] Invalid coordinates. Please enter integer numbers.\n")
                                    gameLoop(board, size, rand, openCoords, currentPlayer, timeLimit, lstBoardsHistory, humanColor)
                            }

                        case "U" =>
                            TUI.showTextPrompt("Are you sure? (y)es or (n)o ")
                            val choice = TUI.getUserInput()
                            choice match {
                                case "Y" =>
                                    if (lstBoardsHistory.isEmpty) {
                                        println("\n[Error] No moves to undo.\n")
                                        gameLoop(board, size, rand, openCoords, currentPlayer, timeLimit, lstBoardsHistory, humanColor)
                                    } else {
                                        val (boardPrev, openCoordsPrev, currentPlayerPrev, randPrev) = lstBoardsHistory.head
                                        gameLoop(boardPrev, size, randPrev, openCoordsPrev, currentPlayerPrev, timeLimit, lstBoardsHistory.tail, humanColor)
                                    }
                                case "N" =>
                                    println("\n")
                                    gameLoop(board, size, rand, openCoords, currentPlayer, timeLimit, lstBoardsHistory, humanColor)
                                case _ =>
                                    println("\n[Error] Invalid option.\n")
                                    gameLoop(board, size, rand, openCoords, currentPlayer, timeLimit, lstBoardsHistory, humanColor)
                            }

                        case "S" =>
                            val fileData = SaveLoadLogic.saveGameState(board, size, currentPlayer, timeLimit, openCoords, rand, humanColor, None, difficulty)                            
                            try {
                                val pw = new java.io.PrintWriter(new java.io.File("savegame.txt"))
                                pw.write(fileData)
                                pw.close()
                                println("\n[Success] Game saved successfully to 'savegame.txt'.\n")
                            } catch {
                                case e: Exception =>
                                    println(s"\n[Error] Failed to save game: ${e.getMessage}\n")
                            }
<<<<<<< Updated upstream
                            gameLoop(board, size, rand, openCoords, currentPlayer, timeLimit, lstBoardsHistory, humanColor)
                        
=======
                            gameLoop(board, size, rand, openCoords, currentPlayer, timeLimit, lstBoardsHistory, humanColor, difficulty)

>>>>>>> Stashed changes
                        case "Q" =>
                            println("\nReturning to Main Menu...")
                            rand

                        case _ =>
                            println("\n[Error] Invalid option.\n")
                            gameLoop(board, size, rand, openCoords, currentPlayer, timeLimit, lstBoardsHistory, humanColor)
                    }
                }
            }
        }
    }
}