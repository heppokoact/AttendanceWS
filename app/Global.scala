import play.api._
import play.api.Play.current
import java.io.File

/**
 * グローバル設定
 */
object Global extends GlobalSettings {
	
  /**
   * サーバー起動時に実行
   */
  override def onStart(app: Application) {
    val tmpDirPath = Play.configuration.getString("play.tmp").get
    new File(tmpDirPath).mkdir()
  }
}