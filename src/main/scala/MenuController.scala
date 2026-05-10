import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.{Parent, Scene, Node}
import javafx.stage.Stage
import javafx.event.ActionEvent
import javafx.scene.control.ComboBox

class MenuController {
  @FXML private var holeComboBox: ComboBox[String] = _

  @FXML
  def initialize(): Unit = {
    holeComboBox.getItems.addAll(
      "Centro",
      "Canto Superior Esquerdo",
      "Canto Superior Direito",
      "Canto Inferior Esquerdo",
      "Canto Inferior Direito"
    )
    // Deixar o "Centro" selecionado por defeito
    holeComboBox.getSelectionModel.selectFirst()
  }

  @FXML def onPlay4x4(e: ActionEvent): Unit = loadGame(4, e)
  @FXML def onPlay6x6(e: ActionEvent): Unit = loadGame(6, e)
  @FXML def onPlay8x8(e: ActionEvent): Unit = loadGame(8, e)
  @FXML def onPlay10x10(e: ActionEvent): Unit = loadGame(10, e)

  private def getSelectedHole(): HolePosition = {
    holeComboBox.getValue match {
      case "Centro" => HolePosition.Center
      case "Canto Superior Esquerdo" => HolePosition.TopLeft
      case "Canto Superior Direito" => HolePosition.TopRight
      case "Canto Inferior Esquerdo" => HolePosition.BottomLeft
      case "Canto Inferior Direito" => HolePosition.BottomRight
      case _ => HolePosition.Center
    }
  }

  // Carrega a vista do jogo e passa o tamanho do tabuleiro
  private def loadGame(size: Int, event: ActionEvent): Unit = {
    val loader = new FXMLLoader(getClass.getResource("Game.fxml"))
    val root: Parent = loader.load()

    val controller = loader.getController[GameController]()
    controller.initGame(size,getSelectedHole())

    val stage = event.getSource.asInstanceOf[Node].getScene.getWindow.asInstanceOf[Stage]
    stage.setScene(new Scene(root))
  }
}