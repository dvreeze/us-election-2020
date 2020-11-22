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

import election.data.Candidate
import election.data.VotingTimeSeries
import election.data.VotingTimeSeries.IndexedVotingSnapshot
import election.report.ReportEntry
import election.report.ReportEntry.CandidateData
import election.report.TimeSeriesReport
import ujson._

/**
 * Creates a JSON report of the time series in the US 2020 election voting data set (of 1 or more states). The report is more informative
 * to a reader than the input JSON, because voting totals (per candidate) are shown, as well as deltas compared to the preceding
 * voting shapshots. This program can create multiple reports for all JSON input directly under an input directory, if an
 * input directory is given instead of an input file.
 *
 * Program arguments: the input JSON (or directory), output directory, optional candidate 1, optional candidate 2.
 *
 * The default candidates 1 and 2 are "trumpd" and "bidenj", respectively.
 *
 * Each output file has the same file name as the input file, but preceded by prefix "report-" in the file name. Each output
 * file is in the JSON format for type TimeSeriesReport.
 *
 * @author Chris de Vreeze
 */
object CreateReport {

  def main(args: Array[String]): Unit = {
    require(args.sizeIs == 2 || args.sizeIs == 4, s"Usage: CreateReport <json data set> <output dir> [<candidate 1> <candidate 2>]")

    val jsonFileOrDir: File = new File(args(0))

    val outputDir: File = new File(args(1))
    outputDir.mkdirs()
    require(outputDir.isDirectory, s"Not a directory: $outputDir")

    val candidate1: Candidate = if (args.sizeIs == 4) Candidate(args(2)) else Candidate.Trump
    val candidate2: Candidate = if (args.sizeIs == 4) Candidate(args(3)) else Candidate.Biden

    val jsonFiles: Seq[File] = if (jsonFileOrDir.isFile) {
      Seq(jsonFileOrDir)
    } else {
      require(jsonFileOrDir.isDirectory, s"Not a directory: $jsonFileOrDir")

      jsonFileOrDir
        .listFiles(f => f.isFile && f.getName.endsWith(".json"))
        .toSeq
        .sortBy(_.getName)
    }

    jsonFiles.foreach { f =>
      println(s"Processing file '$f'")

      Try {
        val report: TimeSeriesReport = createReport(f, candidate1, candidate2)

        val reportJson: Obj = report.toJsonObj

        Using.resource(new FileWriter(new File(outputDir, "report-" + f.getName))) { fw =>
          writeTo(reportJson, fw, indent = 2)
        }

        // Check the report after having written it
        checkReport(report, readTimeSeries(f), candidate1, candidate2)

        // Check lossless roundtripping
        require(TimeSeriesReport.fromJsonObj(reportJson) == report, s"Roundtripping to/from JSON not completely lossless")
      }.recover { case t: Exception => println(s"Exception thrown (but report may have been created): $t") }
    }
  }

  def readTimeSeries(jsonInputFile: File): VotingTimeSeries = {
    val allJsonData: Value = ujson.read(jsonInputFile)

    val timeseriesData: Arr = allJsonData("data")("races")(0)("timeseries").arr

    val timeSeries: VotingTimeSeries = VotingTimeSeries.fromJsonArr(timeseriesData)
    timeSeries
  }

  def createReport(timeSeries: VotingTimeSeries, candidate1: Candidate, candidate2: Candidate): TimeSeriesReport = {
    val expectedCandidates: Set[Candidate] = Set(candidate1, candidate2)

    require(
      timeSeries.snapshots.forall(_.containsAllOfCandidates(expectedCandidates)),
      s"Not every snapshot contains candidates ${expectedCandidates.mkString(", ")}"
    )

    require(timeSeries.nonEmptySnapshots.forall(_.totalVotes > 0L), s"Total votes not always > 0")
    require(timeSeries.nonEmptySnapshots.forall(_.totalVotesOfCandidate(candidate1) > 0L), s"$candidate1 votes not always > 0")
    require(timeSeries.nonEmptySnapshots.forall(_.totalVotesOfCandidate(candidate2) > 0L), s"$candidate2 votes not always > 0")

    require(
      timeSeries.nonEmptySnapshots.forall(_.voteSharesAreWithinBounds),
      s"Vote shares not always within bounds (>= 0 and <= 1, also in total)")

    require(timeSeries.snapshots.headOption.exists(_.isEmpty), s"Time series not starting with 'empty' snapshot")

    val report: TimeSeriesReport = TimeSeriesReport.from(timeSeries, candidate1, candidate2)
    report
  }

  def createReport(jsonInputFile: File, candidate1: Candidate, candidate2: Candidate): TimeSeriesReport = {
    val timeSeries: VotingTimeSeries = readTimeSeries(jsonInputFile)

    if (!timeSeries.isInChronologicalOrder) {
      println(s"${jsonInputFile.getName} not in chronological order (but creating report anyway)!")
    }

    createReport(timeSeries, candidate1, candidate2)
  }

  def checkReport(report: TimeSeriesReport, votingTimeSeries: VotingTimeSeries, candidate1: Candidate, candidate2: Candidate): Unit = {
    report.reportEntries.foreach(entry => checkReportEntry(entry, votingTimeSeries, candidate1, candidate2))

    require(
      report.reportEntries.size == votingTimeSeries.nonEmptySnapshots.size,
      s"Expected ${votingTimeSeries.nonEmptySnapshots.size} report entries instead of ${report.reportEntries.size} ones"
    )
  }

  def checkReportEntry(reportEntry: ReportEntry, votingTimeSeries: VotingTimeSeries, candidate1: Candidate, candidate2: Candidate): Unit = {
    val snapshot: IndexedVotingSnapshot =
      votingTimeSeries.snapshots(reportEntry.originalIndex).ensuring(_.index == reportEntry.originalIndex)

    require(reportEntry.totalVotes == snapshot.totalVotes, s"Different total votes")

    val candidate1Ok = reportEntry.candidate1Data.copy(deltaVotes = 0) == CandidateData(
      candidate1,
      snapshot.voteShareOfCandidate(candidate1),
      snapshot.totalVotesOfCandidateAsBigDecimal(candidate1),
      0)

    require(candidate1Ok, s"Difference in data for $candidate1")

    val candidate2Ok = reportEntry.candidate2Data.copy(deltaVotes = 0) == CandidateData(
      candidate2,
      snapshot.voteShareOfCandidate(candidate2),
      snapshot.totalVotesOfCandidateAsBigDecimal(candidate2),
      0)

    require(candidate2Ok, s"Difference in data for $candidate2")
  }
}
