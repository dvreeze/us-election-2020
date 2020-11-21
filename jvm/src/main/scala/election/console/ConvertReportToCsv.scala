/*
 * Copyright 2020-2020 Chris de Vreeze
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package election.console

import java.io.File
import java.io.PrintWriter

import scala.io.Codec
import scala.util.Try
import scala.util.chaining._

import election.report.ReportEntry
import election.report.TimeSeriesReport
import ujson._

/**
 * Converts a report set in JSON format to CSV. Outputs the CSV result to the given output directory.
 *
 * @author Chris de Vreeze
 */
object ConvertReportToCsv {

  def main(args: Array[String]): Unit = {
    require(args.sizeIs == 2, s"Usage: ConvertReportToCsv <json report data set> <output dir>")

    val jsonReportFileOrDir: File = new File(args(0))

    val outputDir: File = new File(args(1))
    outputDir.mkdirs()
    require(outputDir.isDirectory, s"Not a directory: $outputDir")

    val jsonReportFiles: Seq[File] = if (jsonReportFileOrDir.isFile) {
      Seq(jsonReportFileOrDir)
    } else {
      require(jsonReportFileOrDir.isDirectory, s"Not a directory: $jsonReportFileOrDir")

      jsonReportFileOrDir
        .listFiles(f => f.isFile && f.getName.endsWith(".json"))
        .toSeq
        .sortBy(_.getName)
    }

    jsonReportFiles.foreach { f =>
      println(s"Processing report file '$f'")

      Try {
        val report: TimeSeriesReport = readReport(f)

        val csv: Seq[Seq[String]] = convertReport(report)

        val outputFile: File =
          new File(outputDir, f.getName.ensuring(_.endsWith(".json")).dropRight(5).pipe(_ + ".csv"))

        val pw = new PrintWriter(outputFile, Codec.UTF8.toString)
        writeCsv(csv, pw)
        pw.close()
      }.recover { case t: Exception => println(s"Exception thrown (CSV file may or may not have been created): $t") }
    }
  }

  def readReport(jsonReportInputFile: File): TimeSeriesReport = {
    val allJsonData: Value = ujson.read(jsonReportInputFile)

    val report: TimeSeriesReport = TimeSeriesReport.fromJsonObj(allJsonData.obj)
    report
  }

  def convertReport(report: TimeSeriesReport): Seq[Seq[String]] = {
    val firstEntry: ReportEntry = report.reportEntries.headOption.getOrElse(sys.error(s"Empty report not allowed"))

    val header: Seq[String] = firstEntry.productElementNames
      .take(5)
      .toSeq
      .appendedAll(firstEntry.candidate1Data.productElementNames.drop(1).map(s => s"$s ${firstEntry.candidate1Data.candidate}"))
      .appendedAll(firstEntry.candidate2Data.productElementNames.drop(1).map(s => s"$s ${firstEntry.candidate2Data.candidate}"))
      .appendedAll(firstEntry.thirdPartyData.productElementNames.drop(1).map(s => s"$s ${firstEntry.thirdPartyData.candidate}"))

    val candidateDataFieldCount = firstEntry.candidate1Data.productElementNames.size

    val details: Seq[Seq[String]] = report.reportEntries.map { entry =>
      (0 until 5)
        .map(i => entry.productElement(i).toString)
        .appendedAll(1.until(candidateDataFieldCount).map(i => entry.candidate1Data.productElement(i).toString))
        .appendedAll(1.until(candidateDataFieldCount).map(i => entry.candidate2Data.productElement(i).toString))
        .appendedAll(1.until(candidateDataFieldCount).map(i => entry.thirdPartyData.productElement(i).toString))
    }

    details.prepended(header)
  }

  def writeCsv(csv: Seq[Seq[String]], pw: PrintWriter): Unit = {
    csv.foreach { line =>
      pw.println(line.mkString(columnSeparator))
    }
  }

  private val columnSeparator = ","
}
