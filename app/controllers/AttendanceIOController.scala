package controllers

import play.api._
import play.api.mvc._
import play.api.Play.current
import services.AttendanceImporter
import services.AttendanceExporter
import java.util.concurrent.atomic.AtomicInteger
import java.util.Date
import java.io.File
import services.AttendanceImportError

/**
 * 出欠記録をインポート、エクスポートするコントローラ。
 */
object AttendanceIOController extends Controller {

  private val seqGenerator = new AtomicInteger

  def index = Action {
    Ok(views.html.attendanceIO(Nil, Nil))
  }

  def imp = Action(parse.multipartFormData) { request =>
    request.body.file("attendances").map { attendance =>
      val tmpDir = Play.configuration.getString("play.tmp").get
      val seq = seqGenerator.getAndIncrement()
      val file = new File(s"${tmpDir}/AttendanceImport${seq}.xls")
      attendance.ref.moveTo(file, true)
      val errors = AttendanceImporter.execute(file)
      if (errors.isEmpty) {
        Ok(views.html.attendanceIO(Nil, Array(s"出欠記録のインポートが完了しました。（${"%tF %<tT".format(new Date)}）")))
      } else {
        Ok(views.html.attendanceIO(errors, Nil))
      }
    }.getOrElse {
      val error = AttendanceImportError(None, "出欠記録ファイルを取得できませんでした。アップロードするファイルが正しく指定されているか確認して下さい。")
      Ok(views.html.attendanceIO(Array(error), Nil))
    }
  }

  def exp = Action {
    val file = AttendanceExporter.execute()
    Ok.sendFile(
      content = file,
      fileName = _ => "Attendance.xls")
  }

}