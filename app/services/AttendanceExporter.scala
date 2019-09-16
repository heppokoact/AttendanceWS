package services

import java.io.File
import org.apache.poi.hssf.usermodel.HSSFWorkbook
import org.apache.poi.hssf.util.HSSFCellUtil
import models.Attendance
import play.api.Play
import play.api.Play.current
import java.io.FileOutputStream
import java.util.Date
import java.util.concurrent.atomic.AtomicInteger
import org.apache.poi.hssf.usermodel.HSSFSheet
import org.apache.poi.hssf.usermodel.HSSFRow

/**
 * 出欠記録をExcelにエクスポートするクラス
 */
class AttendanceExporter {
  private val book = {
    val in = Play.classloader.getResourceAsStream("resources/attendanceTemplate.xls")
    try {
      new HSSFWorkbook(in)
    } finally {
      in.close()
    }
  }
  private val sheet = book.getSheetAt(0)
  private val firstDataRow = sheet.getRow(1)
  private lazy val lastRowNum = sheet.getLastRowNum()

  def execute() = {
    // 出欠情報をひな形に書き込み
    writeAttendances()
    // 印刷範囲の設定
    configurePrintArea()
    // 出席人数の数式を設定
    writeSummaries()
    // Excelの書き出し
    outputExcel()
  }

  /**
   * 出欠情報を書き込み
   */
  private def writeAttendances() {
    Attendance.allOrderBySeqNo.zipWithIndex.foreach {
      case (att, i) =>
        val row = HSSFCellUtil.getRow(i + 1, sheet)
        (0 to 13).foreach { colIndex =>
          val cell = HSSFCellUtil.getCell(row, colIndex)
          cell.setCellStyle(firstDataRow.getCell(colIndex).getCellStyle())
          colIndex match {
            case 0 => cell.setCellValue(att.seqNo)
            case 1 => cell.setCellValue(att.empNo)
            case 2 => cell.setCellValue(att.empName)
            case 3 => att.postName.map { cell.setCellValue }
            case 4 => att.grpName.map { cell.setCellValue }
            case 5 => att.pjName.map { cell.setCellValue }
            case 6 => att.workLocation.map {cell.setCellValue }
            case 7 => att.dispCat.map {cell.setCellValue }
            case 8 => att.handoutName.map { cell.setCellValue }
            case 9 => att.rcptName.map { cell.setCellValue }
            case 10 => cell.setCellValue(toAttSitName(att.attCat))
            case 11 => att.remCol.map { cell.setCellValue }
            case 12 => cell.setCellValue(toItemSitName(att.handoutSitCat, att.handoutName))
            case 13 => cell.setCellValue(toItemSitName(att.rcptSitCat, att.rcptName))
          }
        }
    }
  }

  /**
   * 出欠状況を表示名に変換
   */
  private def toAttSitName(attCat: String) = attCat match {
    case "1" => "○"
    case "2" => "×"
    case "3" => "遅"
    case "4" => "早"
    case _ => ""
  }

  /**
   * 配布状況、受取状況を表示名に変換
   */
  private def toItemSitName(sit: (String, Option[String])) = sit match {
    case ("0", Some(itemName)) if !itemName.isEmpty => "×"
    case ("1", _) => "○"
    case _ => ""
  }

  /**
   * 出欠情報の集計を記入
   */
  private def writeSummaries() {
    val summary = List(("出席数", "○"), ("欠席数", "×"), ("遅刻数", "遅"), ("早退数", "早"), ("未記帳数", ""))
    summary.zipWithIndex.foreach {
      case ((title, letter), index) =>
        val summaryRowNum = lastRowNum + 2 + index
        val summaryRow = HSSFCellUtil.getRow(summaryRowNum, sheet)
        val titleCell = HSSFCellUtil.getCell(summaryRow, 9)
        val countCell = HSSFCellUtil.getCell(summaryRow, 10)
        val dataRange = "$K$2:$K$" + (lastRowNum + 1)
        titleCell.setCellValue(title)
        countCell.setCellFormula(letter match {
          case "" => "COUNTBLANK(" + dataRange + ")"
          case _ => "COUNTIF(" + dataRange + ",\"" + letter + "\")"
        })
    }
  }
  
  /**
   * 印刷範囲の設定
   */
  private def configurePrintArea() {
    book.setPrintArea(0, 0, 10, 0, lastRowNum)
  }

  /**
   * Ecxelを書き出し
   */
  private def outputExcel(): File = {
    // 関数を再計算
    book.setForceFormulaRecalculation(true)
    
    // Excelを書き出し
    val tmpDir = Play.configuration.getString("play.tmp").get
    val seq = AttendanceExporter.seqGenerator.getAndIncrement()
    val excel = new File(s"${tmpDir}/AttendanceExport${seq}.xls")
    val out = new FileOutputStream(excel)
    try {
      book.write(out)
    } finally {
      out.close()
    }
    excel
  }
}

object AttendanceExporter {
  /** 一時ディレクトリに出力するExcelに付ける連番 */
  private val seqGenerator = new AtomicInteger()

  def execute(): File = {
    val exporter = new AttendanceExporter()
    exporter.execute()
  }
}