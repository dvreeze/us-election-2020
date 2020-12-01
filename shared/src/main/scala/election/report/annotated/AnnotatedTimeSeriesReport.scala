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

package election.report.annotated

import scala.annotation.tailrec
import scala.collection.immutable.ListMap

import election.data.Candidate
import election.report.ReportEntry
import election.report.TimeSeriesReport
import ujson._

/**
 * Report, annotated with anomalies found.
 *
 * @author Chris de Vreeze
 */
final case class AnnotatedTimeSeriesReport(reportEntries: Seq[AnnotatedReportEntry]) {
  require(reportEntries.nonEmpty, s"No report entries found")

  def candidate1: Candidate = reportEntries(0).candidate1Data.candidate

  def candidate2: Candidate = reportEntries(0).candidate2Data.candidate

  def thirdParty: Candidate = reportEntries(0).thirdPartyData.candidate

  def report: TimeSeriesReport = {
    TimeSeriesReport(reportEntries.map(_.reportEntry))
  }

  def findReportEntryGroupsHavingSameDeltaVoteShare(candidate: Candidate): Seq[Seq[ReportEntry]] = {
    findReportEntryGroupsHavingSameDeltaVoteShare(candidate, startIndex = 0, Seq.empty)
  }

  def anomalies: Seq[String] = {
    ListMap(
      candidate1 -> findReportEntryGroupsHavingSameDeltaVoteShare(candidate1),
      candidate2 -> findReportEntryGroupsHavingSameDeltaVoteShare(candidate2),
      thirdParty -> findReportEntryGroupsHavingSameDeltaVoteShare(thirdParty)
    ).toSeq
      .filter(_._2.nonEmpty)
      .flatMap {
        case (candidate, groups) =>
          groups.map { group =>
            val deltaVoteShare = group(0).deltaVoteSharesPerCandidate(candidate)
            val startIndex = group(0).originalIndex
            s"Candidate $candidate has the same deltaVoteShare of $deltaVoteShare ${group.size} times in succession, starting with original index $startIndex"
          }
      }
  }

  def toJsonObj: Obj = {
    Obj(
      "timeseries" -> Arr(reportEntries.map(_.toJsonObj): _*),
      "anomalies" -> Arr(anomalies.map(v => Str(v)): _*)
    )
  }

  @tailrec
  private def findReportEntryGroupsHavingSameDeltaVoteShare(
      candidate: Candidate,
      startIndex: Int,
      acc: Seq[Seq[ReportEntry]]): Seq[Seq[ReportEntry]] = {
    assert(startIndex >= 0 && startIndex <= reportEntries.size)
    assert(reportEntries(0).reportEntry.deltaVoteSharesPerCandidate.contains(candidate))

    if (startIndex == reportEntries.size) {
      acc
    } else {
      val firstDeltaVoteShare: BigDecimal = reportEntries(startIndex).reportEntry.deltaVoteSharesPerCandidate(candidate)

      val group: Seq[AnnotatedReportEntry] = reportEntries
        .slice(startIndex, reportEntries.size)
        .takeWhile(_.reportEntry.deltaVoteSharesPerCandidate(candidate) == firstDeltaVoteShare)

      // Recursive calls
      if (group.sizeIs <= 1) {
        findReportEntryGroupsHavingSameDeltaVoteShare(candidate, startIndex + 1, acc)
      } else {
        findReportEntryGroupsHavingSameDeltaVoteShare(candidate, startIndex + group.size, acc.appended(group.map(_.reportEntry)))
      }
    }
  }
}

object AnnotatedTimeSeriesReport {

  def from(report: TimeSeriesReport): AnnotatedTimeSeriesReport = {
    AnnotatedTimeSeriesReport(report.reportEntries.map(e => AnnotatedReportEntry.from(e)))
  }
}
