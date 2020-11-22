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
import java.time.ZonedDateTime

import scala.io.Codec
import scala.util.Using
import scala.util.Try
import scala.util.chaining._

import election.data.Candidate
import election.report.ReportEntry
import election.report.TimeSeriesReport
import ujson._

/**
 * Converts a report set in JSON format to CSV. Outputs the CSV result to the given output directory. So the input of this
 * program is the JSON output from program CreateReport, corresponding to type TimeSeriesReport.
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

        Using.resource(new PrintWriter(outputFile, Codec.UTF8.toString)) { pw =>
          writeCsv(csv, pw)
        }
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

    val header: Seq[String] = Line
      .fromReportEntry(firstEntry)
      .toHeader(firstEntry.candidate1Data.candidate, firstEntry.candidate2Data.candidate, firstEntry.thirdPartyData.candidate)

    val details: Seq[Seq[String]] = report.reportEntries.map(entry => Line.fromReportEntry(entry).toDetail)

    details.prepended(header)
  }

  def writeCsv(csv: Seq[Seq[String]], pw: PrintWriter): Unit = {
    csv.foreach { line =>
      pw.println(line.mkString(columnSeparator))
    }
  }

  final case class Line(
      originalIndex: Int,
      timestamp: ZonedDateTime,
      deltaSeconds: Long,
      totalVotes: Long,
      deltaVotes: Long,
      candidate1VoteShare: BigDecimal,
      candidate1TotalVotes: BigDecimal,
      candidate1DeltaVotes: BigDecimal,
      candidate2VoteShare: BigDecimal,
      candidate2TotalVotes: BigDecimal,
      candidate2DeltaVotes: BigDecimal,
      thirdPartyVoteShare: BigDecimal,
      thirdPartyTotalVotes: BigDecimal,
      thirdPartyDeltaVotes: BigDecimal
  ) {

    def toHeader(candidate1: Candidate, candidate2: Candidate, thirdParty: Candidate): Seq[String] = {
      val candidateColumns: Seq[String] = Seq("voteShare", "totalVotes", "deltaVotes")

      this.productElementNames.toSeq
        .take(5)
        .appendedAll(candidateColumns.map(_ + s" $candidate1"))
        .appendedAll(candidateColumns.map(_ + s" $candidate2"))
        .appendedAll(candidateColumns.map(_ + s" $thirdParty"))
    }

    def toDetail: Seq[String] = {
      Seq(
        originalIndex.toString,
        timestamp.toString,
        deltaSeconds.toString,
        totalVotes.toString,
        deltaVotes.toString,
        candidate1VoteShare.toString,
        candidate1TotalVotes.toString,
        candidate1DeltaVotes.toString,
        candidate2VoteShare.toString,
        candidate2TotalVotes.toString,
        candidate2DeltaVotes.toString,
        thirdPartyVoteShare.toString,
        thirdPartyTotalVotes.toString,
        thirdPartyDeltaVotes.toString
      )
    }
  }

  object Line {

    final case class CandidateColumn[A](candidate: Candidate, value: A)

    def fromReportEntry(entry: ReportEntry): Line = {
      Line(
        entry.originalIndex,
        entry.timestamp,
        entry.deltaSeconds,
        entry.totalVotes,
        entry.deltaVotes,
        entry.candidate1Data.voteShare,
        entry.candidate1Data.totalVotes,
        entry.candidate1Data.deltaVotes,
        entry.candidate2Data.voteShare,
        entry.candidate2Data.totalVotes,
        entry.candidate2Data.deltaVotes,
        entry.thirdPartyData.voteShare,
        entry.thirdPartyData.totalVotes,
        entry.thirdPartyData.deltaVotes,
      )
    }
  }

  private val columnSeparator = ","
}
