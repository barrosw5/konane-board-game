import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.layout.{GridPane, StackPane}
import javafx.scene.control.Label
import javafx.scene.shape.Circle
import javafx.scene.paint.Color
import javafx.scene.input.MouseEvent
import javafx.event.ActionEvent
import javafx.scene.{Node, Parent, Scene}
import javafx.stage.Stage

import scala.annotation.tailrec

class GameController {
  @FXML private var boardGrid: GridPane = _
  @FXML private var statusLabel: Label = _

  private var board: Board = _
  private var size: Int = _
  private var currentPlayer: Stone = Stone.Black
  private var openCoords: List[Coord2D] = Nil
  private var selected: Option[Coord2D] = None
  private var isGameOver: Boolean = false

  def initGame(boardSize: Int, hole: HolePosition): Unit = {
    this.size = boardSize
    this.board = GameLogic.initBoard(size, hole)
    val (p1, p2) = hole match {
      case HolePosition.Center =>
        val m = size / 2
        ((m - 1, m - 1), (m - 1, m))
      case HolePosition.TopLeft => ((0, 0), (0, 1))
      case HolePosition.TopRight => ((0, size - 1), (0, size - 2))
      case HolePosition.BottomLeft => ((size - 1, 0), (size - 1, 1))
      case HolePosition.BottomRight => ((size - 1, size - 1), (size - 1, size - 2))
    }
    this.openCoords = List(p1, p2)
    this.currentPlayer = Stone.Black
    this.isGameOver = false
    checkGameOver()
    render()
  }

  def render(): Unit = {
    boardGrid.getChildren.clear()
    drawBoard(0, 0)
  }

  @tailrec
  private def drawBoard(l: Int, c: Int): Unit = {
    if (l >= size) () // Condição de paragem
    else if (c >= size) drawBoard(l + 1, 0) // Avança linha
    else {
      boardGrid.add(createCell(l, c), c, l)
      drawBoard(l, c + 1) // Avança coluna
    }
  }

  private def createCell(l: Int, c: Int): StackPane = {
    val cell = new StackPane()
    cell.setPrefSize(50, 50)

    val coord = (l, c)
    val isSelected = selected.contains(coord)

    // Verifica se esta casa é um destino válido para a peça selecionada
    val isValidDestination = selected match {
      case Some(from) => GameLogic.getValidMovesForPiece(board, from, size).contains(coord)
      case None => false
    }

    // Devolve a cor verde para saberes para onde podes saltar
    val bgColor = if (isSelected) "#ffeb3b"
    else if (isValidDestination) "#a5d6a7"
    else "#d7ccc8"

    cell.setStyle(s"-fx-background-color: $bgColor; -fx-border-color: #5d4037;")

    board.get(coord) match {
      case Some(s) =>
        val circle = new Circle(18)
        circle.setFill(if (s == Stone.Black) Color.BLACK else Color.WHITE)
        cell.getChildren.add(circle)
      case None => () // Casa vazia
    }

    if (!isGameOver) {
      cell.setOnMouseClicked((_: MouseEvent) => handleSelect(l, c))
    }
    cell
  }

  private def handleSelect(l: Int, c: Int): Unit = {
    val clicked = (l, c)

    selected match {
      case None =>
        // Seleciona a peça se existir e pertencer ao jogador atual
        board.get(clicked) match {
          case Some(s) if s == currentPlayer => selected = Some(clicked)
          case _ => ()
        }

      case Some(from) =>
        if (from == clicked) {
          // O jogador clicou na peça que já estava selecionada (amarela) - Termina o turno voluntariamente
          currentPlayer = if (currentPlayer == Stone.Black) Stone.White else Stone.Black
          selected = None
          checkGameOver()
        } else {
          // AQUI ESTAVA O ERRO: O bloco "else" estava em falta!
          // Agora ele verifica corretamente se a casa clicada é um movimento válido
          val moves = GameLogic.getValidMovesForPiece(board, from, size)

          if (moves.contains(clicked)) {
            val (newBoardOpt, newOpen) = GameLogic.play(board, currentPlayer, from, clicked, openCoords)

            newBoardOpt match {
              case Some(newBoard) =>
                board = newBoard
                openCoords = newOpen

                val futureMoves = GameLogic.getValidMovesForPiece(board, clicked, size)
                if (futureMoves.nonEmpty) {
                  // Se ainda tiver saltos, mantém a mesma peça selecionada
                  selected = Some(clicked)
                  statusLabel.setText("Podes saltar outra vez! Continua ou clica na tua peça para parar.")
                  statusLabel.setTextFill(Color.YELLOW)
                } else {
                  // Acabaram os saltos, muda o jogador
                  currentPlayer = if (currentPlayer == Stone.Black) Stone.White else Stone.Black
                  selected = None
                  checkGameOver()
                }
              case None => ()
            }
          } else {
            // Clicou noutra casa (inválida). Se for uma peça do próprio jogador, muda a seleção.
            board.get(clicked) match {
              case Some(s) if s == currentPlayer => selected = Some(clicked)
              case _ => selected = None
            }
          }
        }
    }
    // Atualiza a visualização com o novo estado (quer tenha havido movimento, seleção ou mudança de turno)
    render()
  }

  private def checkGameOver(): Unit = {
    if (!GameLogic.hasValidMoves(board, currentPlayer, size)) {
      isGameOver = true
      val vencedor = if (currentPlayer == Stone.Black) "BRANCO" else "PRETO"
      statusLabel.setText(s"FIM DE JOGO! O jogador $vencedor venceu!")
      statusLabel.setTextFill(Color.RED)
    } else {
      // Atualiza a Label para o turno normal do próximo jogador
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
}