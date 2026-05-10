// Graphical User Interface
// Fazer aqui T6, T8
import javafx.application.Application
import javafx.fxml.FXMLLoader
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage

class GUI extends Application {
  override def start(primaryStage: Stage): Unit = {
    primaryStage.setTitle("Konane")

    // Carrega o layout desenhado
    val fxmlLoader = new FXMLLoader(getClass.getResource("Konane.fxml"))
    val mainViewRoot: Parent = fxmlLoader.load()

    val scene = new Scene(mainViewRoot)
    primaryStage.setScene(scene)
    primaryStage.show()
  }
}

// Para arrancar a interface
object GUIApp {
  def main(args: Array[String]): Unit = {
    Application.launch(classOf[KonaneGUI], args: _*)
  }
}