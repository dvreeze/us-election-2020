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

package election.data

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

import ujson._

/**
 * One entry or snapshot in the voting time series. Only non-empty ones should be counted.
 *
 * @author Chris de Vreeze
 */
final case class VotingSnapshot(
    voteShares: Map[Candidate, BigDecimal],
    totalVotes: Long,
    timestamp: ZonedDateTime,
    otherData: Map[String, String])
    extends VotingSnapshotApi {

  def voteShareOfCandidate(candidate: Candidate): BigDecimal = voteShares.getOrElse(candidate, BigDecimal(0))

  def totalVotesOfCandidate(candidate: Candidate): Long = {
    // Rounding as we normally expect it, so 2.5 is rounded up to 3 while 2.4 is rounded down to 2.
    VotingSnapshot.multiply(voteShareOfCandidate(candidate), totalVotes)
  }

  def totalVotesOfCandidateAsBigDecimal(candidate: Candidate): BigDecimal = {
    voteShareOfCandidate(candidate) * totalVotes
  }

  def containsAllOfCandidates(candidates: Set[Candidate]): Boolean = candidates.subsetOf(voteShares.keySet)

  def voteSharesAreWithinBounds: Boolean = {
    val sum = voteShares.values.sum
    sum >= 0 && sum <= 1 && voteShares.values.forall(_ >= 0) && voteShares.values.forall(_ <= 1)
  }

  def isBefore(other: VotingSnapshotApi): Boolean = timestamp.isBefore(other.timestamp)

  def isAfter(other: VotingSnapshotApi): Boolean = timestamp.isAfter(other.timestamp)

  def isEmpty: Boolean = totalVotes == 0

  def nonEmpty: Boolean = !isEmpty
}

object VotingSnapshot {

  def multiply(share: BigDecimal, total: Long): Long = {
    // Rounding as we normally expect it, so 2.5 is rounded up to 3 while 2.4 is rounded down to 2.
    (share * total).setScale(0, BigDecimal.RoundingMode.HALF_UP).toLong
  }

  def fromJson(jsonObj: Obj): VotingSnapshot = {
    VotingSnapshot(
      jsonObj("vote_shares").obj.toMap.map { case (k, v) => Candidate(k) -> BigDecimal(v.num) },
      jsonObj("votes").num.toLong,
      ZonedDateTime.parse(jsonObj("timestamp").str, DateTimeFormatter.ISO_DATE_TIME),
      Map.empty // TODO
    )
  }

  def gainedTotalVotes(snapshot1: VotingSnapshot, snapshot2: VotingSnapshot): Long = {
    require(snapshot1.isBefore(snapshot2), s"Snapshot $snapshot1 is not before $snapshot2")

    snapshot2.totalVotes - snapshot1.totalVotes
  }

  def gainedVotesOfCandidate(candidate: Candidate, snapshot1: VotingSnapshot, snapshot2: VotingSnapshot): Long = {
    require(snapshot1.isBefore(snapshot2), s"Snapshot $snapshot1 is not before $snapshot2")

    snapshot2.totalVotesOfCandidate(candidate) - snapshot1.totalVotesOfCandidate(candidate)
  }

  def gainedVotesOfCandidateAsBigDecimal(candidate: Candidate, snapshot1: VotingSnapshot, snapshot2: VotingSnapshot): BigDecimal = {
    require(snapshot1.isBefore(snapshot2), s"Snapshot $snapshot1 is not before $snapshot2")

    snapshot2.totalVotesOfCandidateAsBigDecimal(candidate) - snapshot1.totalVotesOfCandidateAsBigDecimal(candidate)
  }

  def gainedVoteShareOfCandidate(candidate: Candidate, snapshot1: VotingSnapshot, snapshot2: VotingSnapshot): BigDecimal = {
    require(snapshot1.isBefore(snapshot2), s"Snapshot $snapshot1 is not before $snapshot2")

    snapshot2.voteShareOfCandidate(candidate) - snapshot1.voteShareOfCandidate(candidate)
  }
}
