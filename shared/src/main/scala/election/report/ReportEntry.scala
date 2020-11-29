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

package election.report

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit

import election.data.Candidate
import election.data.VotingTimeSeries.IndexedVotingSnapshot
import election.report.ReportEntry.CandidateData
import ujson._

import scala.util.chaining._

/**
 * Report entry, corresponding to a voting snapshot and deltas to the preceding voting snapshot.
 *
 * The report entry immediately gives more information to the reader than the original voting snapshot and its predecessor.
 *
 * @author Chris de Vreeze
 */
final case class ReportEntry(
    originalIndex: Int,
    timestamp: ZonedDateTime,
    deltaSeconds: Long,
    totalVotes: Long,
    deltaVotes: Long,
    candidate1Data: CandidateData,
    candidate2Data: CandidateData,
    thirdPartyData: CandidateData
) {

  def voteShareCandidate1: BigDecimal = candidate1Data.voteShare

  def totalVotesCandidate1: BigDecimal = candidate1Data.totalVotes

  def deltaVotesCandidate1: BigDecimal = candidate1Data.deltaVotes

  def voteShareCandidate2: BigDecimal = candidate2Data.voteShare

  def totalVotesCandidate2: BigDecimal = candidate2Data.totalVotes

  def deltaVotesCandidate2: BigDecimal = candidate2Data.deltaVotes

  def voteShareThirdParty: BigDecimal = thirdPartyData.voteShare

  def totalVotesThirdParty: BigDecimal = thirdPartyData.totalVotes

  def deltaVotesThirdParty: BigDecimal = thirdPartyData.deltaVotes

  def containsAllOfCandidates(candidates: Set[Candidate]): Boolean = {
    candidates.subsetOf(Set(candidate1Data.candidate, candidate2Data.candidate))
  }

  def voteSharesPerCandidate: Map[Candidate, BigDecimal] = {
    List(candidate1Data, candidate2Data, thirdPartyData).map(d => d.candidate -> d.voteShare).toMap
  }

  def totalVotesPerCandidate: Map[Candidate, BigDecimal] = {
    List(candidate1Data, candidate2Data, thirdPartyData).map(d => d.candidate -> d.totalVotes).toMap
  }

  def deltaVotesPerCandidate: Map[Candidate, BigDecimal] = {
    List(candidate1Data, candidate2Data, thirdPartyData).map(d => d.candidate -> d.deltaVotes).toMap
  }

  def deltaVoteSharesPerCandidate: Map[Candidate, BigDecimal] = {
    List(candidate1Data, candidate2Data, thirdPartyData).map(d => d.candidate -> d.deltaVoteShare).toMap
  }

  def voteSharesAreWithinBounds: Boolean = {
    val sum = voteSharesPerCandidate.values.sum
    sum >= 0 && sum <= 1 && voteSharesPerCandidate.values.forall(_ >= 0) && voteSharesPerCandidate.values.forall(_ <= 1)
  }

  def toJsonObj: Obj = {
    Obj(
      "original_index" -> Num(originalIndex),
      "timestamp" -> Str(timestamp.toString),
      "delta_seconds" -> Num(deltaSeconds.toDouble),
      "total_votes" -> Num(totalVotes.toDouble),
      "delta_votes" -> Num(deltaVotes.toDouble),
      candidate1Data.toJson,
      candidate2Data.toJson,
      thirdPartyData.toJson,
    )
  }
}

object ReportEntry {

  final case class CandidateData(
      candidate: Candidate,
      voteShare: BigDecimal,
      totalVotes: BigDecimal,
      deltaVotes: BigDecimal,
      deltaVoteShare: BigDecimal) {

    def toJson: (String, Obj) = {
      candidate.toString -> Obj(
        "vote_share" -> Num(voteShare.toDouble),
        "total_votes" -> Num(totalVotes.toDouble),
        "delta_votes" -> Num(deltaVotes.toDouble),
        "delta_vote_share" -> Num(deltaVoteShare.toDouble)
      )
    }
  }

  object CandidateData {

    def from(
        prevSnapshot: IndexedVotingSnapshot,
        snapshot: IndexedVotingSnapshot,
        candidate: Candidate
    ): CandidateData = {
      val deltaVotesOfCandidate: BigDecimal = snapshot.totalVotesOfCandidateAsBigDecimal(candidate) - prevSnapshot
        .totalVotesOfCandidateAsBigDecimal(candidate)
      val totalDeltaVotes: Long = snapshot.totalVotes - prevSnapshot.totalVotes

      CandidateData(
        candidate,
        snapshot.voteShareOfCandidate(candidate).setScale(3, BigDecimal.RoundingMode.HALF_UP),
        snapshot.totalVotesOfCandidateAsBigDecimal(candidate).setScale(3, BigDecimal.RoundingMode.HALF_UP),
        deltaVotesOfCandidate.setScale(3, BigDecimal.RoundingMode.HALF_UP),
        computeDeltaVoteShare(deltaVotesOfCandidate, totalDeltaVotes).getOrElse(BigDecimal(0)) // Correct?
      )
    }

    def fromJson(key: String, jsonObj: Obj): CandidateData = {
      CandidateData(
        Candidate(key),
        BigDecimal(jsonObj("vote_share").num).setScale(3, BigDecimal.RoundingMode.HALF_UP),
        BigDecimal(jsonObj("total_votes").num).setScale(3, BigDecimal.RoundingMode.HALF_UP),
        BigDecimal(jsonObj("delta_votes").num).setScale(3, BigDecimal.RoundingMode.HALF_UP),
        BigDecimal(jsonObj("delta_vote_share").num).setScale(3, BigDecimal.RoundingMode.HALF_UP),
      )
    }

    private[ReportEntry] def computeDeltaVoteShare(deltaVotesOfCandidate: BigDecimal, totalDeltaVotes: Long): Option[BigDecimal] = {
      if (totalDeltaVotes == 0L) {
        None
      } else {
        Some(deltaVotesOfCandidate / BigDecimal(totalDeltaVotes))
      }.pipe(_.map(_.setScale(3, BigDecimal.RoundingMode.HALF_UP)))
    }
  }

  def from(
      prevSnapshot: IndexedVotingSnapshot,
      snapshot: IndexedVotingSnapshot,
      candidate1: Candidate,
      candidate2: Candidate): ReportEntry = {
    val candidate1Data: CandidateData = CandidateData.from(prevSnapshot, snapshot, candidate1)
    val candidate2Data: CandidateData = CandidateData.from(prevSnapshot, snapshot, candidate2)

    val voteShareThirdParty: BigDecimal = BigDecimal(1) - snapshot.voteShareOfCandidate(candidate1) - snapshot.voteShareOfCandidate(
      candidate2)
    val thirdPartyVotes: BigDecimal = voteShareThirdParty * snapshot.totalVotes

    val prevVoteShareThirdParty: BigDecimal = BigDecimal(1) - prevSnapshot.voteShareOfCandidate(candidate1) - prevSnapshot
      .voteShareOfCandidate(candidate2)
    val prevThirdPartyVotes: BigDecimal = prevVoteShareThirdParty * prevSnapshot.totalVotes

    val thirdPartyData: CandidateData =
      CandidateData(
        Candidate("other"),
        voteShareThirdParty,
        thirdPartyVotes,
        thirdPartyVotes - prevThirdPartyVotes,
        BigDecimal(1) - candidate1Data.deltaVoteShare - candidate2Data.deltaVoteShare
      )

    ReportEntry(
      snapshot.index,
      snapshot.timestamp,
      prevSnapshot.timestamp.until(snapshot.timestamp, ChronoUnit.SECONDS),
      snapshot.totalVotes,
      snapshot.totalVotes - prevSnapshot.totalVotes,
      candidate1Data,
      candidate2Data,
      thirdPartyData
    )
  }

  def fromJsonObj(jsonObj: Obj): ReportEntry = {
    val jsonMapEntries: Seq[(String, Value)] = jsonObj.value.toSeq.ensuring(_.sizeIs == 8)

    val candidate1Entry: (String, Value) = jsonMapEntries(5)
    val candidate1Data: CandidateData = CandidateData.fromJson(candidate1Entry._1, candidate1Entry._2.obj)

    val candidate2Entry: (String, Value) = jsonMapEntries(6)
    val candidate2Data: CandidateData = CandidateData.fromJson(candidate2Entry._1, candidate2Entry._2.obj)

    val thirdPartyEntry: (String, Value) = jsonMapEntries(7)
    val thirdPartyData: CandidateData = CandidateData.fromJson(thirdPartyEntry._1, thirdPartyEntry._2.obj)

    ReportEntry(
      jsonObj("original_index").num.toInt,
      ZonedDateTime.parse(jsonObj("timestamp").str, DateTimeFormatter.ISO_DATE_TIME),
      jsonObj("delta_seconds").num.toLong,
      jsonObj("total_votes").num.toLong,
      jsonObj("delta_votes").num.toLong,
      candidate1Data,
      candidate2Data,
      thirdPartyData
    )
  }
}
