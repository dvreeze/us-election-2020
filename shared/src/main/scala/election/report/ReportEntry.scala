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
import java.time.temporal.ChronoUnit

import election.data.Candidate
import election.data.VotingTimeSeries.IndexedVotingSnapshot
import election.report.ReportEntry.CandidateData
import ujson._

/**
 * Report entry, corresponding to a voting snapshot and deltas to the preceding voting snapshot.
 *
 * The report entry immediately gives more information to the reader than the original voting snapshot and its predecessor.
 *
 * @author Chris de Vreeze
 */
final case class ReportEntry(
    index: Int,
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

  def toJson: Obj = {
    Obj(
      "index" -> Num(index),
      "timestamp" -> Str(timestamp.toString),
      "delta_seconds" -> Num(deltaSeconds.toDouble),
      "total_votes" -> Num(totalVotes.toDouble),
      "delta_votes" -> Num(deltaVotes.toDouble),
      candidate1Data.toJsonWithKey,
      candidate2Data.toJsonWithKey,
      thirdPartyData.toJsonWithKey,
    )
  }
}

object ReportEntry {

  final case class CandidateData(candidate: Candidate, voteShare: BigDecimal, totalVotes: BigDecimal, deltaVotes: BigDecimal) {

    def toJsonWithKey: (String, Obj) = {
      candidate.toString -> Obj(
        "vote_share" -> Num(voteShare.toDouble),
        "total_votes" -> Num(totalVotes.toDouble),
        "delta_votes" -> Num(deltaVotes.toDouble))
    }
  }

  object CandidateData {

    def from(
        prevSnapshot: IndexedVotingSnapshot,
        snapshot: IndexedVotingSnapshot,
        candidate: Candidate
    ): CandidateData = {
      CandidateData(
        candidate,
        snapshot.voteShareOfCandidate(candidate),
        snapshot.totalVotesOfCandidateAsBigDecimal(candidate),
        snapshot.totalVotesOfCandidateAsBigDecimal(candidate) - prevSnapshot.totalVotesOfCandidateAsBigDecimal(candidate)
      )
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
      CandidateData(Candidate("other"), voteShareThirdParty, thirdPartyVotes, thirdPartyVotes - prevThirdPartyVotes)

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
}
