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
import java.io.FileWriter

import scala.util.Try
import scala.util.Using

import election.report.TimeSeriesReport
import ujson._

import election.report.annotated.AnnotatedTimeSeriesReport

/**
 * Finds certain anomalies in the reports generated from the US 2020 election voting data set. The reports are in the JSON format
 * for type TimeSeriesReport. The output is the same JSON reports, yet annotated with those anomalies.
 *
 * @author Chris de Vreeze
 */
object CreateAnnotatedReport {

  def main(args: Array[String]): Unit = {
    require(args.sizeIs == 2, s"Usage: CreateAnnotatedReport <json report data set> <output dir>")

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
        val annotatedReport: AnnotatedTimeSeriesReport = AnnotatedTimeSeriesReport.from(report)

        val annotatedReportJson: Obj = annotatedReport.toJsonObj

        val outputFile: File = new File(outputDir, "annotated-" + f.getName)

        Using.resource(new FileWriter(outputFile)) { fw =>
          writeTo(annotatedReportJson, fw, indent = 2)
        }

        require(annotatedReport.report == report, s"Removing the annotations does not yield the same as the input report")
      }.recover { case t: Exception => println(s"Exception thrown (report may or may not have been created): $t") }
    }
  }

  def readReport(jsonReportInputFile: File): TimeSeriesReport = {
    val allJsonData: Value = ujson.read(jsonReportInputFile)

    val report: TimeSeriesReport = TimeSeriesReport.fromJsonObj(allJsonData.obj)
    report
  }
}
