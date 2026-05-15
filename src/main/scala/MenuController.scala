import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.{Parent, Scene, Node}
import javafx.stage.Stage
import javafx.event.ActionEvent
import javafx.scene.control.ComboBox
import javafx.scene.control.TextField

class MenuController {
  @FXML private var holeComboBox: ComboBox[String] = _
  @FXML private var timeLimitField: TextField = _
  @FXML private var colorComboBox: ComboBox[String] = _
  @FXML private var sizeComboBox: ComboBox[String] = _


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

    sizeComboBox.getItems.addAll("4", "6", "8", "10")
    sizeComboBox.getSelectionModel.selectFirst()

    colorComboBox.getItems.addAll(
      "Jogar com Pretas",
      "Jogar com Brancas",
      "Dois Humanos (PvP)"
    )
    colorComboBox.getSelectionModel.selectFirst()
  }


  @FXML def onStartGame(event: ActionEvent): Unit = {
    val size = sizeComboBox.getValue.toInt
    loadGame(size, event)
  }
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
    controller.initGame(size, getSelectedHole(), getTimeLimit(),getHumanColor())

    val stage = event.getSource.asInstanceOf[Node].getScene.getWindow.asInstanceOf[Stage]
    stage.setScene(new Scene(root))
  }

  @FXML def onLoadGame(event: ActionEvent): Unit = {
    val loader = new FXMLLoader(getClass.getResource("Game.fxml"))
    val root: Parent = loader.load()
    val controller = loader.getController[GameController]()
    controller.loadGameFromSave() // método novo que inicializa a partir do ficheiro
    val stage = event.getSource.asInstanceOf[Node].getScene.getWindow.asInstanceOf[Stage]
    stage.setScene(new Scene(root))
  }


  private def getTimeLimit(): Long = {
    try {
      val secs = timeLimitField.getText.toDouble
      if (secs > 0) (secs * 1000).toLong else 30000L // 30 segundos default
    } catch {
      case _: NumberFormatException => 30000L
    }
  }

  private def getHumanColor(): Option[Stone] = {
    colorComboBox.getValue match {
      case "Jogar com Pretas" => Some(Stone.Black)
      case "Jogar com Brancas" => Some(Stone.White)
      case _ => None // Dois Humanos
    }
  }
}