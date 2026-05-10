import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.{Parent, Scene, Node}
import javafx.stage.Stage
import javafx.event.ActionEvent

class MenuController {

  @FXML def onPlay4x4(e: ActionEvent): Unit = loadGame(4, e)
  @FXML def onPlay6x6(e: ActionEvent): Unit = loadGame(6, e)
  @FXML def onPlay8x8(e: ActionEvent): Unit = loadGame(8, e)
  @FXML def onPlay10x10(e: ActionEvent): Unit = loadGame(10, e)

  // Carrega a vista do jogo e passa o tamanho do tabuleiro
  private def loadGame(size: Int, event: ActionEvent): Unit = {
    val loader = new FXMLLoader(getClass.getResource("Game.fxml"))
    val root: Parent = loader.load()

    val controller = loader.getController[GameController]()
    controller.initGame(size)

    val stage = event.getSource.asInstanceOf[Node].getScene.getWindow.asInstanceOf[Stage]
    stage.setScene(new Scene(root))
  }
}