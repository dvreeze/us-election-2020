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

import java.time.ZonedDateTime

import election.data.Candidate
import election.report.ReportEntry
import election.report.ReportEntry.CandidateData
import election.report.annotated.AnnotatedReportEntry.AnnotatedCandidateData
import election.report.annotated.AnnotatedReportEntry.AnomaliesKey
import ujson._

/**
 * Report entry, annotated with anomalies found.
 *
 * @author Chris de Vreeze
 */
final case class AnnotatedReportEntry(
    originalIndex: Int,
    timestamp: ZonedDateTime,
    deltaSeconds: Long,
    totalVotes: Long,
    deltaVotes: Long,
    candidate1Data: AnnotatedCandidateData,
    candidate2Data: AnnotatedCandidateData,
    thirdPartyData: AnnotatedCandidateData
) {

  def reportEntry: ReportEntry = {
    ReportEntry(
      originalIndex,
      timestamp,
      deltaSeconds,
      totalVotes,
      deltaVotes,
      candidate1Data.candidateData,
      candidate2Data.candidateData,
      thirdPartyData.candidateData)
  }

  def hasNegativeTotalVotes: Boolean = totalVotes < 0

  def hasTotalVotesEqualToZero: Boolean = totalVotes == 0

  def hasNegativeDeltaVotes: Boolean = deltaVotes < 0

  def hasDeltaVotesEqualToZero: Boolean = deltaVotes == 0

  def anomalies: Seq[String] = {
    Seq(
      hasNegativeTotalVotes -> s"Negative total of votes",
      hasTotalVotesEqualToZero -> s"Total of votes of 0",
      hasNegativeDeltaVotes -> s"Negative delta of votes",
      hasDeltaVotesEqualToZero -> s"Delta of votes of 0",
    ).filter(_._1).map(_._2)
  }

  def toJsonObj: Obj = {
    val obj = reportEntry.toJsonObj

    obj(candidate1Data.candidate.toString) = candidate1Data.toJson._2
    obj(candidate2Data.candidate.toString) = candidate2Data.toJson._2
    obj(thirdPartyData.candidate.toString) = thirdPartyData.toJson._2

    if (anomalies.nonEmpty) {
      obj.value += AnomaliesKey -> Arr(anomalies.map(v => Str(v)): _*)
    }

    obj
  }
}

object AnnotatedReportEntry {

  final case class AnnotatedCandidateData(candidateData: CandidateData) {

    def candidate: Candidate = candidateData.candidate

    def hasNegativeTotalVotes: Boolean = candidateData.totalVotes < 0

    def hasTotalVotesEqualToZero: Boolean = candidateData.totalVotes == 0

    def hasNegativeDeltaVotes: Boolean = candidateData.deltaVotes < 0

    def hasDeltaVotesEqualToZero: Boolean = candidateData.deltaVotes == 0

    def anomalies: Seq[String] = {
      Seq(
        hasNegativeTotalVotes -> s"Candidate ${candidateData.candidate} has a negative total of votes",
        hasTotalVotesEqualToZero -> s"Candidate ${candidateData.candidate} has a total of votes of 0",
        hasNegativeDeltaVotes -> s"Candidate ${candidateData.candidate} has a negative delta of votes",
        hasDeltaVotesEqualToZero -> s"Candidate ${candidateData.candidate} has a delta of votes of 0",
      ).filter(_._1).map(_._2)
    }

    def toJson: (String, Obj) = {
      val (objKey, obj) = candidateData.toJson

      if (anomalies.nonEmpty) {
        obj.value += AnomaliesKey -> Arr(anomalies.map(v => Str(v)): _*)
      }

      objKey -> obj
    }
  }

  def from(reportEntry: ReportEntry): AnnotatedReportEntry = {
    AnnotatedReportEntry(
      reportEntry.originalIndex,
      reportEntry.timestamp,
      reportEntry.deltaSeconds,
      reportEntry.totalVotes,
      reportEntry.deltaVotes,
      AnnotatedCandidateData(reportEntry.candidate1Data),
      AnnotatedCandidateData(reportEntry.candidate2Data),
      AnnotatedCandidateData(reportEntry.thirdPartyData),
    )
  }

  private val AnomaliesKey = "anomalies"
}
