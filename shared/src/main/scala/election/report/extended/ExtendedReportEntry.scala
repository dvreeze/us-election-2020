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

package election.report.extended

import java.time.ZonedDateTime

import scala.math.BigDecimal.RoundingMode

import election.data.Candidate
import election.report.ReportEntry
import election.report.ReportEntry.CandidateData
import election.report.extended.ExtendedReportEntry.ExtendedCandidateData
import ujson._

/**
 * Report entry, extended with more derived data.
 *
 * @author Chris de Vreeze
 */
final case class ExtendedReportEntry(
    originalIndex: Int,
    timestamp: ZonedDateTime,
    deltaSeconds: Long,
    totalVotes: Long,
    deltaVotes: Long,
    candidate1Data: ExtendedCandidateData,
    candidate2Data: ExtendedCandidateData,
    thirdPartyData: ExtendedCandidateData
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

  def candidateDataMap: Map[Candidate, ExtendedCandidateData] = {
    Seq(candidate1Data, candidate2Data, thirdPartyData).map(d => d.candidate -> d).toMap
  }

  def candidate1: Candidate = candidate1Data.candidate

  def candidate2: Candidate = candidate2Data.candidate

  def thirdParty: Candidate = thirdPartyData.candidate

  def toJsonObj: Obj = {
    val obj = reportEntry.toJsonObj

    obj(candidate1Data.candidate.toString) = candidate1Data.toJson(deltaVotes)._2
    obj(candidate2Data.candidate.toString) = candidate2Data.toJson(deltaVotes)._2
    obj(thirdPartyData.candidate.toString) = thirdPartyData.toJson(deltaVotes)._2

    obj
  }
}

object ExtendedReportEntry {

  final case class ExtendedCandidateData(candidateData: CandidateData) {

    def candidate: Candidate = candidateData.candidate

    def percentageOfDeltaVotes(deltaVotes: Long): BigDecimal = {
      if (deltaVotes == 0L) {
        BigDecimal(Long.MaxValue)
      } else {
        ((candidateData.deltaVotes / BigDecimal(deltaVotes)) * BigDecimal(100)).setScale(2, RoundingMode.HALF_UP)
      }
    }

    def toJson(deltaVotes: Long): (String, Obj) = {
      val (objKey, obj) = candidateData.toJson

      obj.value += PercentageOfDeltaVotesKey -> Num(percentageOfDeltaVotes(deltaVotes).toDouble)

      objKey -> obj
    }
  }

  def from(reportEntry: ReportEntry): ExtendedReportEntry = {
    ExtendedReportEntry(
      reportEntry.originalIndex,
      reportEntry.timestamp,
      reportEntry.deltaSeconds,
      reportEntry.totalVotes,
      reportEntry.deltaVotes,
      ExtendedCandidateData(reportEntry.candidate1Data),
      ExtendedCandidateData(reportEntry.candidate2Data),
      ExtendedCandidateData(reportEntry.thirdPartyData),
    )
  }

  private val PercentageOfDeltaVotesKey = "percentage_of_delta_votes"
}
