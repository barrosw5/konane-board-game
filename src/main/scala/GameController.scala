import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.layout.{GridPane, StackPane}
import javafx.scene.control.Label
import javafx.scene.shape.Circle
import javafx.scene.paint.Color
import javafx.scene.input.MouseEvent
import javafx.event.ActionEvent
import javafx.scene.{Node, Parent, Scene}
import javafx.stage.Stage
import javafx.animation.{Timeline,KeyFrame}
import javafx.util.Duration

import scala.annotation.tailrec

class GameController {
  @FXML private var boardGrid: GridPane = _
  @FXML private var statusLabel: Label = _
  @FXML private var timerLabel: Label = _

  private var board: Board = _
  private var size: Int = _
  private var currentPlayer: Stone = Stone.Black
  private var openCoords: List[Coord2D] = Nil
  private var selected: Option[Coord2D] = None
  private var isGameOver: Boolean = false

  private var processingMove: Boolean = false

  private val humanColor: Stone = Stone.Black
  private val computerColor: Stone = Stone.White
  private var history: List[(Board, List[Coord2D], Stone, MyRandom)] = Nil
  private var rand: MyRandom = MyRandom(System.currentTimeMillis())

  private var isInJump: Boolean = false
  private var timeLimitMs: Long = 30000L
  private var timer: Option[javafx.animation.Timeline] = None
  private var timeRemainingMs: Long = 0L



  def initGame(boardSize: Int, hole: HolePosition, timeLimitMs: Long): Unit = {
    this.size = boardSize
    this.board = GameLogic.initBoard(size, hole)
    this.timeLimitMs = timeLimitMs

    val (p1, p2) = hole match {
      case HolePosition.Center =>
        val m = size / 2
        ((m - 1, m - 1), (m - 1, m))
      case HolePosition.TopLeft => ((0, 0), (0, 1))
      case HolePosition.TopRight => ((0, size - 1), (0, size - 2))
      case HolePosition.BottomLeft => ((size - 1, 0), (size - 1, 1))
      case HolePosition.BottomRight => ((size - 1, size - 1), (size - 1, size - 2))
    }
    this.selected = None
    this.openCoords = List(p1, p2)
    this.currentPlayer = Stone.Black // Pretas começam
    this.isGameOver = false
    this.processingMove = false

    checkGameOver()
    render()

    if (!isGameOver && isComputerTurn) {
      computerMove()
    }
  }

  private def isComputerTurn: Boolean = currentPlayer == computerColor

  private def pushHistory(): Unit = {
    history = (board, openCoords, currentPlayer, rand) :: history
  }

  private def computerMove(): Unit = {
    if (currentPlayer != computerColor){
      processingMove = false
      return
    }
    if (processingMove || isGameOver) return
    stopTimer()
    processingMove = true
    statusLabel.setText("Computador a pensar...")
    statusLabel.setTextFill(Color.CYAN)

    // Pequeno atraso para ver a jogada do computador
    val delay = new javafx.animation.Timeline(
      new javafx.animation.KeyFrame(javafx.util.Duration.millis(1500), (_: ActionEvent) => {
        doComputerMove()
      })
    )
    delay.setCycleCount(1)
    delay.play()
  }

  private def doComputerMove(): Unit = {
    if (isGameOver) {
      processingMove = false
      return
    }

    pushHistory()

    val (newBoardOpt, newRand, newOpen, _) =
      GameLogic.playRandomly(board, rand, currentPlayer, openCoords, GameLogic.randomMove)

    newBoardOpt match {
      case Some(newBoard) =>
        board = newBoard
        openCoords = newOpen
        rand = newRand
        selected = None
        // Passa a vez
        currentPlayer = if (currentPlayer == Stone.Black) Stone.White else Stone.Black

      case None =>
        // Computador não tem jogadas válidas
        isGameOver = true
        val winner = if (currentPlayer == Stone.Black) "BRANCO" else "PRETO"
        statusLabel.setText(s"Computador sem movimentos! $winner venceu!")
        statusLabel.setTextFill(Color.RED)
    }
    processingMove = false
    render()
    checkGameOver()

    if (!isGameOver && !isComputerTurn) {
      startTimerIfHumanTurn()
      statusLabel.setText(s"Turno: ${if (currentPlayer == Stone.Black) "PRETO" else "BRANCO"}")
      statusLabel.setTextFill(Color.WHITE)
    }
  }

  // Termina o turno
  private def finishHumanTurn(): Unit = {
    selected = None
    isInJump = false
    currentPlayer = if (currentPlayer == Stone.Black) Stone.White else Stone.Black
    checkGameOver()
    render()
    stopTimer()
    if (!isGameOver && isComputerTurn) {
      computerMove()
    } else if (!isGameOver) {
      startTimerIfHumanTurn()
      statusLabel.setText(s"Turno: ${if (currentPlayer == Stone.Black) "PRETO" else "BRANCO"}")
      statusLabel.setTextFill(Color.WHITE)
    }
  }

  private def render(): Unit = {
    boardGrid.getChildren.clear()
    drawBoard(0, 0)
  }

  @tailrec
  private def drawBoard(l: Int, c: Int): Unit = {
    if (l >= size) ()
    else if (c >= size) drawBoard(l + 1, 0)
    else {
      boardGrid.add(createCell(l, c), c, l)
      drawBoard(l, c + 1)
    }
  }

  private def createCell(l: Int, c: Int): StackPane = {
    val cell = new StackPane()
    cell.setPrefSize(50, 50)

    val coord = (l, c)
    val isSelected = selected.contains(coord)
    val isValidDestination = selected match {
      case Some(from) => GameLogic.getValidMovesForPiece(board, from, size).contains(coord)
      case None => false
    }

    val bgColor = if (isSelected) "#ffeb3b"
    else if (isValidDestination) "#a5d6a7"
    else "#d7ccc8"

    cell.setStyle(s"-fx-background-color: $bgColor; -fx-border-color: #5d4037;")

    board.get(coord) match {
      case Some(s) =>
        val circle = new Circle(18)
        circle.setFill(if (s == Stone.Black) Color.BLACK else Color.WHITE)
        cell.getChildren.add(circle)
      case None => ()
    }

    if (!isGameOver && !processingMove && currentPlayer == humanColor) {
      cell.setOnMouseClicked((_: MouseEvent) => handleSelect(l, c))
    }
    cell
  }

  private def handleSelect(l: Int, c: Int): Unit = {
    if (isGameOver || processingMove || currentPlayer != humanColor) return
    val clicked = (l, c)

    selected match {
      case None =>
        if (!isInJump){
          board.get(clicked) match {
            case Some(s) if s == currentPlayer => selected = Some(clicked)
            case _ => ()
          }
    }

      case Some(from) =>
        if (from == clicked) {
          finishHumanTurn()
        } else {
          val moves = GameLogic.getValidMovesForPiece(board, from, size)
          if (moves.contains(clicked)) {
            pushHistory()
            val (newBoardOpt, newOpen) = GameLogic.play(board, currentPlayer, from, clicked, openCoords)
            newBoardOpt match {
              case Some(newBoard) =>
                board = newBoard
                openCoords = newOpen
                val futureMoves = GameLogic.getValidMovesForPiece(board, clicked, size)
                if (futureMoves.nonEmpty) {
                  selected = Some(clicked)
                  isInJump = true
                  statusLabel.setText("Podes saltar outra vez! Clique na peça para parar.")
                  statusLabel.setTextFill(Color.YELLOW)
                } else {
                  isInJump = false
                  finishHumanTurn()
                }
              case None => ()
            }
          } else {
            if (isInJump){
              statusLabel.setText("Clica na mesma peça para parar ou continua os saltos com a mesma peça")
              statusLabel.setTextFill(Color.ORANGE)
            }else{
            board.get(clicked) match {
              case Some(s) if s == currentPlayer => selected = Some(clicked)
              case _ => selected = None
            }
            }
          }
        }
    }
    render()
  }

  private def checkGameOver(): Unit = {
    if (!GameLogic.hasValidMoves(board, currentPlayer, size)) {
      isGameOver = true
      val vencedor = if (currentPlayer == Stone.Black) "BRANCO" else "PRETO"
      statusLabel.setText(s"FIM DE JOGO! O jogador $vencedor venceu!")
      statusLabel.setTextFill(Color.RED)
    } else {
      val corTexto = if (currentPlayer == Stone.Black) "PRETO" else "BRANCO"
      statusLabel.setText(s"Turno: $corTexto")
      statusLabel.setTextFill(Color.WHITE)
    }
  }

  @FXML def onBackToMenu(event: ActionEvent): Unit = {
    val root = FXMLLoader.load[Parent](getClass.getResource("Menu.fxml"))
    val stage = event.getSource.asInstanceOf[Node].getScene.getWindow.asInstanceOf[Stage]
    stage.setScene(new Scene(root))
  }

  @FXML def onUndo(event: ActionEvent): Unit = {
    if (selected.isDefined && isInJump) {
      statusLabel.setText("Não podes fazer undo a meio de uma jogada! Termina os saltos primeiro.")
      statusLabel.setTextFill(Color.ORANGE)
      return
    }
    if (isGameOver) {
      statusLabel.setText("O Jogo Terminou. Não é possível fazer Undo")
      return
    }

    if (processingMove) {
      statusLabel.setText("Aguarda a jogada do computador terminar.")
      return
    }


    history match {
      case Nil => statusLabel.setText("Nada para dar Undo")
      case (b, oc, p, r) :: rest =>
        board = b
        openCoords = oc
        currentPlayer = p
        rand = r
        history = rest
        selected = None
        isInJump = false
        processingMove = false
        render()
        checkGameOver()
        stopTimer()
        statusLabel.setText(s"Undo efetuado. Turno: ${if (currentPlayer == Stone.Black) "PRETO" else "BRANCO"}")
        if (!isGameOver && currentPlayer == computerColor) {
          val delay = new javafx.animation.Timeline(
            new javafx.animation.KeyFrame(javafx.util.Duration.millis(1000), (_: ActionEvent) => {
              if (!isGameOver && currentPlayer == computerColor && !processingMove) {
                computerMove()
              }
            })
          )
          delay.setCycleCount(1)
          delay.play()
        }
    }
  }

  private def startTimerIfHumanTurn(): Unit = {
    stopTimer()
    if (!isGameOver && !isComputerTurn) {
      timeRemainingMs = timeLimitMs
      updateTimerLabel()
      val timeline = new Timeline(
        new KeyFrame(Duration.seconds(1), (_: ActionEvent) => tick())
      )
      timeline.setCycleCount(-1)
      timeline.play()
      timer = Some(timeline)
    } else {
      if (timerLabel != null) timerLabel.setText("Tempo: -- s")
    }
  }

  private def stopTimer(): Unit = {
    timer.foreach(_.stop())
    timer = None
  }

  private def tick(): Unit = {
    if (!isGameOver && !isComputerTurn) {
      timeRemainingMs -= 1000
      updateTimerLabel()
      if (timeRemainingMs <= 0) {
        stopTimer()
        timeoutLose()
      }
    } else {
      stopTimer()
    }
  }

  private def updateTimerLabel(): Unit = {
    if (timerLabel != null) {
      val seconds = math.max(0, timeRemainingMs / 1000)
      timerLabel.setText(f"Tempo: $seconds%d s")
      if (seconds <= 5) timerLabel.setTextFill(Color.RED)
      else timerLabel.setTextFill(Color.YELLOW)
    }
  }

  private def timeoutLose(): Unit = {
    isGameOver = true
    val winner = if (currentPlayer == Stone.Black) "BRANCO" else "PRETO"
    statusLabel.setText(s"TEMPO ESGOTADO! $winner venceu!")
    statusLabel.setTextFill(Color.RED)
    stopTimer()
  }
}