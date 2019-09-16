package controllers

import play.api._
import play.api.libs.json._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.data.validation.Constraints._
import models.AttendanceFormatProtocol
import models.Attendance
import models.AttendanceCondition
import views.html.defaultpages.badRequest

/**
 * 出欠情報を提供するWEBサービスのコントローラ。
 */
object AttendanceController extends Controller with AttendanceFormatProtocol {

  /**
   * 検索系のActionを実行する際にリクエストパラメータをバインドするフォーム
   */
  val findForm = Form(
    mapping(
      "id" -> optional(longNumber),
      "attCat" -> optional(list(text)),
      "handoutSitCat" -> optional(list(text)),
      "rcptSitCat" -> optional(list(text)),
      "pjName" -> optional(text),
      "empName" -> optional(text),
      "empNoFrom" -> optional(text),
      "empNoTo" -> optional(text),
      "order" -> optional(list(text)))(AttendanceCondition.apply)(AttendanceCondition.unapply))

  /**
   * 更新系のActionを実行する際にリクエストパラメータをバインドするフォーム
   */
  val updateForm = Form(
    mapping(
      "id" -> longNumber,
      "attCat" -> nonEmptyText,
      "dispCat" -> optional(text),
      "empName" -> nonEmptyText,
      "empNo" -> nonEmptyText,
      "grpName" -> optional(text),
      "handoutName" -> optional(text),
      "handoutSitCat" -> nonEmptyText,
      "pjName" -> optional(text),
      "workLocation" -> optional(text),
      "postName" -> optional(text),
      "rcptName" -> optional(text),
      "rcptSitCat" -> nonEmptyText,
      "remCol" -> optional(text),
      "seqNo" -> longNumber,
      "timeStamp" -> date("yyyy-MM-dd HH:mm:ss.SSS"))(Attendance.apply)(Attendance.unapply))

  /**
   * リクエストパラメータに合致する出欠情報を取得します。
   */
  def find = Action { implicit request =>
    findForm.bindFromRequest.fold(
      errors => BadRequest("ERROR"),
      condition => Ok(Json.toJson(Attendance.find(condition))).as("application/json"))
  }

  /**
   * 引数のIDの出欠情報を取得します。
   */
  def findById(id: Long) = Action {
    Ok(Json.toJson(Attendance.findById(id)))
  }

  /** 更新が成功した時の戻り値のJSON */
  private val UPDATE_SUCCESS_JSON = """
{
  "statusCode": "200",
  "errors": null
}
"""

  /** 更新が失敗（楽観的排他エラー）した時の戻り値のJSON */
  private val UPDATE_FAILURE_JSON = """
{
  "statusCode": "500",
  "errors": [
    {
      "errorCode": "101",
      "errorMessage": "楽観的排他エラーにより更新が失敗しました。"
    }
  ]
}
"""

  /**
   * リクエストパラメータで出欠情報を更新します。
   */
  def update = Action { implicit request =>
    updateForm.bindFromRequest.fold(
      errors => BadRequest(errors.errors.map(e => s"${e.key}: ${e.message}").mkString(", ")),
      attendance => {
        val result = Attendance.update(attendance) match {
          case 0 => UPDATE_FAILURE_JSON
          case _ => UPDATE_SUCCESS_JSON
        }
        Ok(result).as("application/json")
      })
  }
  
  /**
   * 出欠情報に含まれる全てのプロジェクトを重複を排除して取得します。
   */
  def project = Action {
    val result = Attendance.project();
    Ok(Json.toJson(result)).as("application/json")
  }

}