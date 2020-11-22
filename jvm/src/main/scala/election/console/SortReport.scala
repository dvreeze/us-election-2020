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
import scala.util.chaining._

import election.report.ReportEntry
import election.report.TimeSeriesReport
import ujson._

/**
 * Sorts a report set in JSON format by the given sort criteria. Outputs the sorted result to the given output directory.
 *
 * The input reports are in the JSON format for type TimeSeriesReport. That is, the input of this program is the output
 * from program CreateReport.
 *
 * @author Chris de Vreeze
 */
object SortReport {

  def main(args: Array[String]): Unit = {
    require(args.sizeIs == 3, s"Usage: SortReport <json report data set> <output dir> <sort criteria name>")

    val jsonReportFileOrDir: File = new File(args(0))

    val outputDir: File = new File(args(1))
    outputDir.mkdirs()
    require(outputDir.isDirectory, s"Not a directory: $outputDir")

    val sortCriteria: SortCriteria = SortCriteria.from(args(2))

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
        val sortedReport: TimeSeriesReport = report.sortByDesc(sortCriteria.sortFunction)

        val sortedReportJson: Obj = sortedReport.toJsonObj

        val fileName = f.getName.ensuring(_.endsWith(".json")).dropRight(5).pipe(s => s"sorted-$s-$sortCriteria.json")

        Using(new FileWriter(new File(outputDir, fileName))) { fw =>
          writeTo(sortedReportJson, fw, indent = 2)
        }
      }.recover { case t: Exception => println(s"Exception thrown (report may or may not have been created): $t") }
    }
  }

  def readReport(jsonReportInputFile: File): TimeSeriesReport = {
    val allJsonData: Value = ujson.read(jsonReportInputFile)

    val report: TimeSeriesReport = TimeSeriesReport.fromJsonObj(allJsonData.obj)
    report
  }

  sealed trait SortCriteria {

    def sortFunction: ReportEntry => BigDecimal
  }

  case object MaxDeltaVotes extends SortCriteria {

    def sortFunction: ReportEntry => BigDecimal = { e: ReportEntry =>
      e.deltaVotes
    }
  }

  case object MaxDeltaVotesCandidate1 extends SortCriteria {

    def sortFunction: ReportEntry => BigDecimal = { e: ReportEntry =>
      e.deltaVotesCandidate1
    }
  }

  case object MaxDeltaVotesCandidate2 extends SortCriteria {

    def sortFunction: ReportEntry => BigDecimal = { e: ReportEntry =>
      e.deltaVotesCandidate2
    }
  }

  case object MaxDeltaVotesThirdParty extends SortCriteria {

    def sortFunction: ReportEntry => BigDecimal = { e: ReportEntry =>
      e.deltaVotesThirdParty
    }
  }

  /**
   * Max difference between delta votes of the 2 main candidates, ignoring the 3rd party.
   */
  case object MaxDiffDeltaVotes extends SortCriteria {

    def sortFunction: ReportEntry => BigDecimal = { e: ReportEntry =>
      (e.deltaVotesCandidate1 - e.deltaVotesCandidate2).abs
    }
  }

  object SortCriteria {

    val allCriteria: Set[SortCriteria] =
      Set(MaxDeltaVotes, MaxDeltaVotesCandidate1, MaxDeltaVotesCandidate2, MaxDeltaVotesThirdParty, MaxDiffDeltaVotes)

    def from(s: String): SortCriteria = {
      allCriteria.find(_.toString == s).getOrElse(sys.error(s"Unknown sort criteria: $s"))
    }
  }
}
