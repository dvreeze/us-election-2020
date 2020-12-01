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

import ujson.Obj

/**
 * One entry or vote dump in the voting time series. Only non-empty ones should be counted.
 *
 * @author Chris de Vreeze
 */
final case class VoteDump(
    voteShares: Map[Candidate, BigDecimal],
    totalVotes: Long,
    timestamp: ZonedDateTime,
    otherData: Map[String, String])
    extends VoteDumpApi {

  def voteShareOfCandidate(candidate: Candidate): BigDecimal = voteShares.getOrElse(candidate, BigDecimal(0))

  def totalVotesOfCandidate(candidate: Candidate): Long = {
    // Rounding as we normally expect it, so 2.5 is rounded up to 3 while 2.4 is rounded down to 2.
    VoteDump.multiply(voteShareOfCandidate(candidate), totalVotes)
  }

  def totalVotesOfCandidateAsBigDecimal(candidate: Candidate): BigDecimal = {
    voteShareOfCandidate(candidate) * totalVotes
  }

  def containsAllOfCandidates(candidates: Set[Candidate]): Boolean = candidates.subsetOf(voteShares.keySet)

  def voteSharesAreWithinBounds: Boolean = {
    val sum = voteShares.values.sum
    sum >= 0 && sum <= 1 && voteShares.values.forall(_ >= 0) && voteShares.values.forall(_ <= 1)
  }

  def isBefore(other: VoteDumpApi): Boolean = timestamp.isBefore(other.timestamp)

  def isAfter(other: VoteDumpApi): Boolean = timestamp.isAfter(other.timestamp)

  def isEmpty: Boolean = totalVotes == 0

  def nonEmpty: Boolean = !isEmpty
}

object VoteDump {

  def multiply(share: BigDecimal, total: Long): Long = {
    // Rounding as we normally expect it, so 2.5 is rounded up to 3 while 2.4 is rounded down to 2.
    (share * total).setScale(0, BigDecimal.RoundingMode.HALF_UP).toLong
  }

  def fromJsonObj(jsonObj: Obj): VoteDump = {
    VoteDump(
      jsonObj("vote_shares").obj.toMap.map { case (k, v) => Candidate(k) -> BigDecimal(v.num) },
      jsonObj("votes").num.toLong,
      ZonedDateTime.parse(jsonObj("timestamp").str, DateTimeFormatter.ISO_DATE_TIME),
      Map.empty // TODO
    )
  }

  def gainedTotalVotes(voteDump1: VoteDump, voteDump2: VoteDump): Long = {
    require(voteDump1.isBefore(voteDump2), s"Vote dump $voteDump1 is not before $voteDump2")

    voteDump2.totalVotes - voteDump1.totalVotes
  }

  def gainedVotesOfCandidate(candidate: Candidate, voteDump1: VoteDump, voteDump2: VoteDump): Long = {
    require(voteDump1.isBefore(voteDump2), s"Vote dump $voteDump1 is not before $voteDump2")

    voteDump2.totalVotesOfCandidate(candidate) - voteDump1.totalVotesOfCandidate(candidate)
  }

  def gainedVotesOfCandidateAsBigDecimal(candidate: Candidate, voteDump1: VoteDump, voteDump2: VoteDump): BigDecimal = {
    require(voteDump1.isBefore(voteDump2), s"Vote dump $voteDump1 is not before $voteDump2")

    voteDump2.totalVotesOfCandidateAsBigDecimal(candidate) - voteDump1.totalVotesOfCandidateAsBigDecimal(candidate)
  }

  def gainedVoteShareOfCandidate(candidate: Candidate, voteDump1: VoteDump, voteDump2: VoteDump): BigDecimal = {
    require(voteDump1.isBefore(voteDump2), s"Vote dump $voteDump1 is not before $voteDump2")

    voteDump2.voteShareOfCandidate(candidate) - voteDump1.voteShareOfCandidate(candidate)
  }
}
