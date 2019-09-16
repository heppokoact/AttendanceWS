package models

import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current
import play.api.libs.json._
import play.api.libs.json.Writes._
import java.util.Date
import utils.QueryUtils._

/**
 * 出欠情報を取得するModel。
 */
case class Attendance(
  id: Long,
  attCat: String,
  dispCat: Option[String],
  empName: String,
  empNo: String,
  grpName: Option[String],
  handoutName: Option[String],
  handoutSitCat: String,
  pjName: Option[String],
  workLocation: Option[String],
  postName: Option[String],
  rcptName: Option[String],
  rcptSitCat: String,
  remCol: Option[String],
  seqNo: Long,
  timeStamp: Date)

object Attendance {

  /** DBから取得した出欠情報をAttendanceに変換する関数 */
  val attendance = {
    get[Long]("id") ~
      get[String]("att_cat") ~
      get[Option[String]]("disp_cat") ~
      get[String]("emp_name") ~
      get[String]("emp_no") ~
      get[Option[String]]("grp_name") ~
      get[Option[String]]("handout_name") ~
      get[String]("handout_sit_cat") ~
      get[Option[String]]("pj_name") ~
      get[Option[String]]("work_location") ~
      get[Option[String]]("post_name") ~
      get[Option[String]]("rcpt_name") ~
      get[String]("rcpt_sit_cat") ~
      get[Option[String]]("rem_col") ~
      get[Long]("seq_no") ~
      get[Date]("time_stamp") map {
        case id ~
          attCat ~
          dispCat ~
          empName ~
          empNo ~
          grpName ~
          handoutName ~
          handoutSitCat ~
          pjName ~
          workLocation ~
          postName ~
          rcptName ~
          rcptSitCat ~
          remCol ~
          seqNo ~
          timeStamp => Attendance(id,
          attCat,
          dispCat,
          empName,
          empNo,
          grpName,
          handoutName,
          handoutSitCat,
          pjName,
          workLocation,
          postName,
          rcptName,
          rcptSitCat,
          remCol,
          seqNo,
          timeStamp)
      }
  }

  /**
   * 出欠情報を検索する
   */
  def find(condition: AttendanceCondition): List[Attendance] = DB.withConnection { implicit c =>
    val id = condition.id.map { id => ("id = {id}", List('id -> id)) }
    val attCat = condition.attCat match {
      case None => None
      case Some(Nil) => None
      case Some(other) => Some(toInClause("att_cat", other))
    }
    val handoutSitCat = (condition.handoutSitCat match {
      case None => None
      case Some(Nil) => None
      case Some(other) => Some(toInClause("handout_sit_cat", other))
    }) map {
      case (query, param) => (s"$query AND handout_name != '' ", param)
    }
    val rcptSitCat = (condition.rcptSitCat match {
      case None => None
      case Some(Nil) => None
      case Some(other) => Some(toInClause("rcpt_sit_cat", other))
    }) map {
      case (query, param) => (s"$query AND rcpt_name != '' ", param)
    }
    val pjName = condition.pjName.map { pjName => ("pj_name = {pj_name}", List('pj_name -> pjName)) }
    val empName = condition.empName.map {
      empName => ("emp_name like {emp_name}", List('emp_name -> (s"%$empName%")))
    }
    val empNoFrom = condition.empNoFrom.map {
      empNoFrom => ("emp_no >= {emp_no_from}", List('emp_no_from -> empNoFrom))
    }
    val empNoTo = condition.empNoTo.map {
      empNoTo => ("emp_no <= {emp_no_to}", List('emp_no_to -> empNoTo))
    }

    val handoutAndRcptSitCat = (handoutSitCat, rcptSitCat) match {
      case (None, None) => None
      case (h, None) => h
      case (None, r) => r
      case (Some((hQuery, hParam)), Some((rQuery, rParam))) =>
        Option((s"(($hQuery) OR ($rQuery))", hParam ::: rParam))
    }

    val whereAndParams = List[Option[(String, List[(Symbol, Any)])]](
      id, attCat, handoutAndRcptSitCat, pjName, empName, empNoFrom, empNoTo).flatten
    val where = whereAndParams.map(_._1) match {
      case Nil => ""
      case a => a.mkString(" WHERE ", " AND ", " ")
    }
    val params = whereAndParams.map(_._2).flatten.map(v => v._1 -> toParameterValue(v._2))

    val order = condition.order match {
      case None => "emp_no ASC"
      case Some(orderList) => orderList.mkString(",")
    }

    SQL("""
SELECT
  id,
  att_cat,
  disp_cat,
  emp_name,
  emp_no,
  grp_name,
  handout_name,
  handout_sit_cat,
  pj_name,
  work_location,
  post_name,
  rcpt_name,
  rcpt_sit_cat,
  rem_col,
  seq_no,
  time_stamp
FROM
  attendance
      """ + //
      where + //
      """
ORDER BY
""" + //
      order).on(params: _*).as(attendance *)
  }

  /**
   * 引数の条件で出欠記録を取得する
   */
  def findBy(
    id: Option[Long] = None,
    attCat: Option[List[String]] = None,
    handoutSitCat: Option[List[String]] = None,
    rcptSitCat: Option[List[String]] = None,
    pjName: Option[String] = None,
    empName: Option[String] = None,
    empNoFrom: Option[String] = None,
    empNoTo: Option[String] = None,
    order: Option[List[String]] = None) = find(new AttendanceCondition(
    id,
    attCat,
    handoutSitCat,
    rcptSitCat,
    pjName,
    empName,
    empNoFrom,
    empNoTo,
    order))

  /**
   * IDで出欠情報を検索する
   */
  def findById(id: Long): Option[Attendance] = {
    val attendances = findBy(
      id = Some(id))
    (attendances: @unchecked) match {
      case Nil => None
      case List(att) => Some(att)
    }
  }

  /**
   * 全ての出欠記録を取得します。
   */
  def all: List[Attendance] = findBy()

  /**
   * 全ての出欠記録をシーケンス番号で並び替えて取得します。
   */
  def allOrderBySeqNo = findBy(order = Some(List("seq_no ASC")))

  /**
   * 出欠情報を更新する
   */
  def update(att: Attendance): Int = {
    var result = 0;
    DB.withConnection { implicit c =>
      result = SQL("""
UPDATE attendance SET
  att_cat = {att_cat},
  disp_cat = {disp_cat},
  emp_name = {emp_name},
  emp_no = {emp_no},
  grp_name = {grp_name},
  handout_name = {handout_name},
  handout_sit_cat = {handout_sit_cat},
  pj_name = {pj_name},
  post_name = {post_name},
  rcpt_name = {rcpt_name},
  rcpt_sit_cat = {rcpt_sit_cat},
  rem_col = {rem_col},
  seq_no = {seq_no},
  time_stamp = now()
WHERE
  id = {id}
  AND time_stamp = {time_stamp}
          """).on(
        'id -> att.id,
        'att_cat -> att.attCat,
        'disp_cat -> att.dispCat.getOrElse(""),
        'emp_name -> att.empName,
        'emp_no -> att.empNo,
        'grp_name -> att.grpName.getOrElse(""),
        'handout_name -> att.handoutName.getOrElse(""),
        'handout_sit_cat -> att.handoutSitCat,
        'pj_name -> att.pjName.getOrElse(""),
        'post_name -> att.postName.getOrElse(""),
        'rcpt_name -> att.rcptName.getOrElse(""),
        'rcpt_sit_cat -> att.rcptSitCat,
        'rem_col -> att.remCol.getOrElse(""),
        'seq_no -> att.seqNo,
        'time_stamp -> att.timeStamp).executeUpdate()
    }

    result
  }

  /**
   * 出欠情報に含まれる全てのプロジェクトを重複を排除して取得します。
   */
  def project(): List[String] = {
    DB.withConnection { implicit c =>
      val result = SQL("""
SELECT
  pj_name
FROM
  attendance
WHERE
  pj_name != ''
GROUP BY
  pj_name
ORDER BY
  pj_name
 """)().map({
        case Row(pjName: String) => pjName
      }).toList
      return result;
    }
  }

  /**
   * 出欠記録を全て削除し、引数の出欠記録を挿入する
   */
  def replaceAllAttendances(attendances: Seq[Attendance]) {
    DB.withTransaction { implicit c =>
      // 出欠記録を全て削除
      SQL("DELETE FROM attendance").executeUpdate()
      // 引数の出欠記録を全て挿入
      val insertQuery = SQL(
        """
INSERT INTO attendance (
  att_cat,
  disp_cat,
  emp_name,
  emp_no,
  grp_name,
  handout_name,
  handout_sit_cat,
  pj_name,
  work_location,
  post_name,
  rcpt_name,
  rcpt_sit_cat,
  rem_col,
  seq_no,
  time_stamp
) VALUES (
  {att_cat},
  {disp_cat},
  {emp_name},
  {emp_no},
  {grp_name},
  {handout_name},
  {handout_sit_cat},
  {pj_name},
  {work_location},
  {post_name},
  {rcpt_name},
  {rcpt_sit_cat},
  {rem_col},
  {seq_no},
  now()
)
""")

      val batchQuery = (insertQuery.asBatch /: attendances) {
        (query, attendance) =>
          query.addBatchParams(
            attendance.attCat,
            attendance.dispCat.getOrElse(""),
            attendance.empName,
            attendance.empNo,
            attendance.grpName.getOrElse(""),
            attendance.handoutName.getOrElse(""),
            attendance.handoutSitCat,
            attendance.pjName.getOrElse(""),
            attendance.workLocation.getOrElse(""),
            attendance.postName.getOrElse(""),
            attendance.rcptName.getOrElse(""),
            attendance.rcptSitCat,
            attendance.remCol.getOrElse(""),
            attendance.seqNo)
      }
      batchQuery.execute()
    }
  }

}

/**
 * 出欠情報を取得するための条件
 */
case class AttendanceCondition(
  val id: Option[Long],
  val attCat: Option[List[String]],
  val handoutSitCat: Option[List[String]],
  val rcptSitCat: Option[List[String]],
  val pjName: Option[String],
  val empName: Option[String],
  val empNoFrom: Option[String],
  val empNoTo: Option[String],
  val order: Option[List[String]])

/**
 * 出欠情報をJSONに変換するプロトコル
 */
trait AttendanceFormatProtocol {
  implicit object AttendanceFormat extends Writes[Attendance] {
    def writes(att: Attendance): JsValue =
      JsObject(List(
        "id" -> JsNumber(att.id),
        "attCat" -> JsString(att.attCat),
        "dispCat" -> JsString(att.dispCat.getOrElse("")),
        "empName" -> JsString(att.empName),
        "empNo" -> JsString(att.empNo),
        "grpName" -> JsString(att.grpName.getOrElse("")),
        "handoutName" -> JsString(att.handoutName.getOrElse("")),
        "handoutSitCat" -> JsString(att.handoutSitCat),
        "pjName" -> JsString(att.pjName.getOrElse("")),
        "postName" -> JsString(att.postName.getOrElse("")),
        "rcptName" -> JsString(att.rcptName.getOrElse("")),
        "rcptSitCat" -> JsString(att.rcptSitCat),
        "remCol" -> JsString(att.remCol.getOrElse("")),
        "seqNo" -> JsNumber(att.seqNo),
        "timeStamp" -> JsString("%tF %<tT.%<tL".format(att.timeStamp))))
  }

}