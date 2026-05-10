import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.layout.{GridPane, StackPane}
import javafx.scene.control.Label
import javafx.scene.shape.Circle
import javafx.scene.paint.Color
import javafx.scene.input.MouseEvent
import javafx.event.ActionEvent
import javafx.scene.{Parent, Scene, Node}
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

  def initGame(boardSize: Int): Unit = {
    this.size = boardSize
    this.board = GameLogic.initBoard(size, HolePosition.Center)
    val m = size / 2
    this.openCoords = List((m - 1, m - 1), (m - 1, m))
    render()
  }

  def render(): Unit = {
    boardGrid.getChildren.clear()
    statusLabel.setText(s"Turno: ${if (currentPlayer == Stone.Black) "PRETO" else "BRANCO"}")
    drawBoard(0, 0)
  }

  @tailrec
  private def drawBoard(l: Int, c: Int): Unit = {
    if (l >= size) () // Condição de paragem (fim do tabuleiro)
    else if (c >= size) drawBoard(l + 1, 0) // Avança para a próxima linha
    else {
      boardGrid.add(createCell(l, c), c, l)
      drawBoard(l, c + 1) // Avança para a próxima coluna
    }
  }

  private def createCell(l: Int, c: Int): StackPane = {
    val cell = new StackPane()
    cell.setPrefSize(50, 50)

    val isSelected = selected.contains((l, c))
    val bgColor = if (isSelected) "#ffeb3b" else "#d7ccc8"
    cell.setStyle(s"-fx-background-color: $bgColor; -fx-border-color: #5d4037;")

    board.get((l, c)) match {
      case Some(s) =>
        val circle = new Circle(18)
        circle.setFill(if (s == Stone.Black) Color.BLACK else Color.WHITE)
        cell.getChildren.add(circle)
      case None => () // Casa vazia, não desenha nada
    }

    cell.setOnMouseClicked((_: MouseEvent) => handleSelect(l, c))
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
        val moves = GameLogic.getValidMovesForPiece(board, from, size)

        if (moves.contains(clicked)) {
          val (newBoardOpt, newOpen) = GameLogic.play(board, currentPlayer, from, clicked, openCoords)

          newBoardOpt match {
            case Some(newBoard) =>
              board = newBoard
              openCoords = newOpen
              currentPlayer = if (currentPlayer == Stone.Black) Stone.White else Stone.Black
              selected = None
            case None => ()
          }
        } else {
          // Permite mudar a seleção para outra peça própria, caso contrário anula a seleção
          board.get(clicked) match {
            case Some(s) if s == currentPlayer => selected = Some(clicked)
            case _ => selected = None
          }
        }
    }

    // Atualiza a visualização com o novo estado
    render()
  }

  @FXML def onBackToMenu(event: ActionEvent): Unit = {
    val root = FXMLLoader.load[Parent](getClass.getResource("Menu.fxml"))
    val stage = event.getSource.asInstanceOf[Node].getScene.getWindow.asInstanceOf[Stage]
    stage.setScene(new Scene(root))
  }
}