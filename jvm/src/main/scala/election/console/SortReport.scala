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

import election.report.ReportEntry
import election.report.TimeSeriesReport
import ujson._

/**
 * Sorts a report in JSON format by the given sort criteria. Outputs the sorted result to the console.
 *
 * @author Chris de Vreeze
 */
object SortReport {

  def main(args: Array[String]): Unit = {
    require(
      args.sizeIs == 2,
      s"Usage: SortReport <json report> <sort criteria name>")

    val jsonReportFile: File = new File(args(0))

    val sortCriteria: SortCriteria = SortCriteria.from(args(1))

    val report: TimeSeriesReport = readReport(jsonReportFile)

    val sortedReport: TimeSeriesReport = report.sortByDesc(sortCriteria.sortFunction)

    println(write(sortedReport.toJsonObj, 2))
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

    def sortFunction: ReportEntry => BigDecimal = { (e: ReportEntry) => e.deltaVotes }
  }

  case object MaxDeltaVotesCandidate1 extends SortCriteria {

    def sortFunction: ReportEntry => BigDecimal = { (e: ReportEntry) => e.deltaVotesCandidate1 }
  }

  case object MaxDeltaVotesCandidate2 extends SortCriteria {

    def sortFunction: ReportEntry => BigDecimal = { (e: ReportEntry) => e.deltaVotesCandidate2 }
  }

  case object MaxDeltaVotesThirdParty extends SortCriteria {

    def sortFunction: ReportEntry => BigDecimal = { (e: ReportEntry) => e.deltaVotesThirdParty }
  }

  // ...

  object SortCriteria {

    def from(s: String): SortCriteria = {
      s match {
        case "MaxDeltaVotes" => MaxDeltaVotes
        case "MaxDeltaVotesCandidate1" => MaxDeltaVotesCandidate1
        case "MaxDeltaVotesCandidate2" => MaxDeltaVotesCandidate2
        case "MaxDeltaVotesThirdParty" => MaxDeltaVotesThirdParty
        case _ => sys.error(s"Unknown sort criteria: $s")
      }
    }
  }
}
