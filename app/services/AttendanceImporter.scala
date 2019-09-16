package services

import play.api._
import play.api.Play.current
import play.api.db._
import java.io.File
import org.apache.poi.hssf.usermodel.HSSFWorkbook
import org.apache.poi.hssf.util.HSSFCellUtil
import org.apache.poi.hssf.usermodel.HSSFSheet
import org.apache.poi.hssf.usermodel.HSSFRow
import org.apache.poi.ss.usermodel.Cell
import java.io.FileInputStream
import scala.collection.mutable.MutableList
import scala.util.control.Breaks._
import models.Attendance
import scala.collection.mutable.ListBuffer
import java.util.Date
import scala.collection.mutable.LinkedHashMap
import java.io.IOException
import org.apache.poi.EncryptedDocumentException

/**
 * インポートするデータにエラーが有った場合のエラーデータ
 */
case class AttendanceImportError(
  line: Option[Int],
  message: String)

/**
 * 出欠記録をExcelからインポートするクラス
 */
class AttendanceImporter(file: File) {

  private case class ImportData(
    attCat: String,
    dispCat: String,
    empName: String,
    empNo: String,
    grpName: String,
    handoutName: String,
    handoutSitCat: String,
    pjName: String,
    workLocation: String,
    postName: String,
    rcptName: String,
    rcptSitCat: String,
    remCol: String,
    seqNo: String,
    rowNum: Int) {

    val attCatMaster = LinkedHashMap("" -> "0", "○" -> "1", "×" -> "2", "遅" -> "3", "早" -> "4")
    val itemCatMaster = LinkedHashMap("" -> "0", "○" -> "1", "×" -> "0")

    private def isAvailable {
      !seqNo.isEmpty && seqNo != "0"
    }

    def check {
      val reqiredCheck = (name: String, value: String) => {
        if (value.isEmpty()) errors += AttendanceImportError(Some(rowNum), s"${name}は必ず入力してください。")
      }
      val digitCheck = (name: String, value: String) => {
        if (!value.isEmpty() && !value.forall(_.isDigit))
          errors += AttendanceImportError(Some(rowNum), s"${name}は数字で入力してください。")
      }
      def maxLengthCheck(max: Int)(name: String, value: String) = {
        if (!value.isEmpty() && value.length > max)
          errors += AttendanceImportError(Some(rowNum), s"${name}は${max}文字以内で入力してください。")
      }
      def masterCheck(master: scala.collection.Map[String, String])(name: String, value: String) = {
        if (!value.isEmpty() && !master.contains(value)) {
          val masterStr = master.keysIterator.map { elem => if (elem.isEmpty()) "空白" else elem } mkString (",")
          errors += AttendanceImportError(Some(rowNum), s"${name}には[${masterStr}]のいずれかを入力してください。")
        }
      }

      List(digitCheck).foreach(_("項番", seqNo))
      List(reqiredCheck, maxLengthCheck(5)_, digitCheck).foreach(_("社員番号", empNo))
      List(reqiredCheck).foreach(_("氏名", empName))
      List(masterCheck(attCatMaster)_).foreach(_("出欠", attCat))
      List(masterCheck(itemCatMaster)_).foreach(_("配布状況", handoutSitCat))
      List(masterCheck(itemCatMaster)_).foreach(_("受取状況", rcptSitCat))
      
      if (handoutName.isEmpty && handoutSitCat == "○") {
        errors += AttendanceImportError(Some(rowNum), s"配布物名が空の場合、配布状況は「○」にできません。")
      }
      if (rcptName.isEmpty && rcptSitCat == "○") {
        errors += AttendanceImportError(Some(rowNum), s"受取物名が空の場合、受取状況は「○」にできません。")
      }
    }

    def toAttendance:Attendance = {
      Attendance(
        id = DUMMY_ID,
        seqNo = seqNo.toLong,
        empNo = empNo.reverse.padTo(5, "0").reverse.mkString,
        empName = empName,
        postName = Some(postName),
        grpName = Some(grpName),
        pjName = Some(pjName),
        workLocation = Some(workLocation),
        dispCat = Some(dispCat),
        handoutName = Some(handoutName),
        rcptName = Some(rcptName),
        attCat = attCatMaster.get(attCat).get,
        remCol = Some(remCol),
        handoutSitCat = itemCatMaster.get(handoutSitCat).get,
        rcptSitCat = itemCatMaster.get(rcptSitCat).get,
        timeStamp = DUMMY_TIMESTAMP)
    }
  }

  private val errors = ListBuffer[AttendanceImportError]()

  private val book = {
    val in = new FileInputStream(file)
    try {
      new HSSFWorkbook(in)
    } catch {
      case e:IOException => { 
        errors += AttendanceImportError(None, "ファイルの読み込みに失敗しました。アップロードするファイルを正しく指定して下さい。")
        null
      }
      case e:EncryptedDocumentException => {
        errors += AttendanceImportError(None, "ファイルにパスワードが指定されているため、読み込みに失敗しました。パスワードを削除して下さい。")
        null
      }
    } finally {
      in.close()
    }
  }
  private val sheet = if (book != null) book.getSheetAt(0) else null

  private val DUMMY_ID = -1
  private val DUMMY_TIMESTAMP = new Date(0)

  def execute(): Seq[AttendanceImportError] = {
    if (!errors.isEmpty) return errors

    val datas = for {
      rowNum <- 1 to sheet.getLastRowNum()
      row = HSSFCellUtil.getRow(rowNum, sheet)
      seqNo = getCellValueAsString(row, 0)
      if "" != seqNo && "0" != seqNo
    } yield ImportData(
      seqNo = seqNo,
      empNo = getCellValueAsString(row, 1),
      empName = getCellValueAsString(row, 2),
      postName = getCellValueAsString(row, 3),
      grpName = getCellValueAsString(row, 4),
      pjName = getCellValueAsString(row, 5),
      workLocation = getCellValueAsString(row, 6),
      dispCat = getCellValueAsString(row, 7),
      handoutName = getCellValueAsString(row, 8),
      rcptName = getCellValueAsString(row, 9),
      attCat = getCellValueAsString(row, 10),
      remCol = getCellValueAsString(row, 11),
      handoutSitCat = getCellValueAsString(row, 12),
      rcptSitCat = getCellValueAsString(row, 13),
      rowNum = rowNum)
    
    if (datas.isEmpty) {
        errors += AttendanceImportError(None, "出欠記録が１件もありません。アップロードするファイルが正しく指定して下さい。")
    }

    if (!errors.isEmpty) return errors

    datas.foreach(_.check)

    if (!errors.isEmpty) return errors

    val attendances = datas.map(_.toAttendance)
    Attendance.replaceAllAttendances(attendances)

    Nil
  }

  private def getSeqNo(row: HSSFRow): Long = {
    row.getCell(0).getNumericCellValue().toLong
  }

  private def getCellValueAsString(row: HSSFRow, colNum: Int): String = {
    try {
      val cell = HSSFCellUtil.getCell(row, colNum)
      if (cell == null) return ""
      cell.getCellType match {
        case Cell.CELL_TYPE_BLANK => ""
        case Cell.CELL_TYPE_BOOLEAN => cell.getBooleanCellValue().toString
        case Cell.CELL_TYPE_ERROR => throw new RuntimeException
        case Cell.CELL_TYPE_FORMULA => {
          val evaluator = book.getCreationHelper.createFormulaEvaluator()
          evaluator.evaluateInCell(cell)
          getCellValueAsString(row, colNum)
        }
        case Cell.CELL_TYPE_NUMERIC => cell.getNumericCellValue().toLong.toString
        case Cell.CELL_TYPE_STRING => cell.getStringCellValue().trim()
      }
    } catch {
      case e: Throwable => {
        errors += AttendanceImportError(Some(row.getRowNum()), s"${colNum}列目のセルの読み取り中にエラーが発生しました。")
        ""
      }
    }
  }

}

object AttendanceImporter {
  def execute(file: File): Seq[AttendanceImportError] = {
    val importer = new AttendanceImporter(file)
    importer.execute()
  }
}

