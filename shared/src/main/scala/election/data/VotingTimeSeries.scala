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

import election.data.VotingTimeSeries.IndexedVoteDump
import ujson._

/**
 * The voting time series
 *
 * @author Chris de Vreeze
 */
final case class VotingTimeSeries(voteDumps: Seq[IndexedVoteDump]) extends AnyVal {

  def nonEmptyVoteDumps: Seq[IndexedVoteDump] = voteDumps.filter(_.nonEmpty)

  /**
   * Returns true if all non-empty vote dumps are in chronological order. If 2 subsequent vote dumps have the same timestamp,
   * they are still considered to be in chronological order.
   */
  def isInChronologicalOrder: Boolean = {
    nonEmptyVoteDumps.sliding(2).filter(_.sizeIs == 2).forall { xs =>
      !xs.ensuring(_.sizeIs == 2)(0).isAfter(xs(1))
    }
  }

  def sortedChronologically: VotingTimeSeries = VotingTimeSeries(voteDumps.sortBy(_.timestamp))

  def voteDumpPairs: Seq[(IndexedVoteDump, IndexedVoteDump)] = {
    voteDumps.sliding(2).filter(_.sizeIs == 2).map(xs => xs(0) -> xs(1)).toSeq
  }

  def nonEmptyVoteDumpPairs: Seq[(IndexedVoteDump, IndexedVoteDump)] = {
    nonEmptyVoteDumps.sliding(2).filter(_.sizeIs == 2).map(xs => xs(0) -> xs(1)).toSeq
  }
}

object VotingTimeSeries {

  /**
   * Vote dump with zero-based index in a VotingTimeSeries. Only non-empty ones should be counted.
   */
  final case class IndexedVoteDump(voteDump: VoteDump, index: Int) extends VoteDumpApi {

    def voteShares: Map[Candidate, BigDecimal] = voteDump.voteShares

    def voteShareOfCandidate(candidate: Candidate): BigDecimal = voteDump.voteShareOfCandidate(candidate)

    def totalVotes: Long = voteDump.totalVotes

    def timestamp: ZonedDateTime = voteDump.timestamp

    def otherData: Map[String, String] = voteDump.otherData

    def totalVotesOfCandidate(candidate: Candidate): Long = voteDump.totalVotesOfCandidate(candidate)

    def totalVotesOfCandidateAsBigDecimal(candidate: Candidate): BigDecimal = voteDump.totalVotesOfCandidateAsBigDecimal(candidate)

    def containsAllOfCandidates(candidates: Set[Candidate]): Boolean = voteDump.containsAllOfCandidates(candidates)

    def voteSharesAreWithinBounds: Boolean = voteDump.voteSharesAreWithinBounds

    def isBefore(other: VoteDumpApi): Boolean = voteDump.isBefore(other)

    def isAfter(other: VoteDumpApi): Boolean = voteDump.isAfter(other)

    def isEmpty: Boolean = voteDump.isEmpty

    def nonEmpty: Boolean = voteDump.nonEmpty
  }

  object IndexedVoteDump {

    def gainedTotalVotes(voteDump1: IndexedVoteDump, voteDump2: IndexedVoteDump): Long = {
      VoteDump.gainedTotalVotes(voteDump1.voteDump, voteDump2.voteDump)
    }

    def gainedVotesOfCandidate(candidate: Candidate, voteDump1: IndexedVoteDump, voteDump2: IndexedVoteDump): Long = {
      VoteDump.gainedVotesOfCandidate(candidate, voteDump1.voteDump, voteDump2.voteDump)
    }

    def gainedVotesOfCandidateAsBigDecimal(candidate: Candidate, voteDump1: IndexedVoteDump, voteDump2: IndexedVoteDump): BigDecimal = {
      VoteDump.gainedVotesOfCandidateAsBigDecimal(candidate, voteDump1.voteDump, voteDump2.voteDump)
    }

    def gainedVoteShareOfCandidate(candidate: Candidate, voteDump1: IndexedVoteDump, voteDump2: IndexedVoteDump): BigDecimal = {
      VoteDump.gainedVoteShareOfCandidate(candidate, voteDump1.voteDump, voteDump2.voteDump)
    }
  }

  def fromJsonArr(jsonArr: Arr): VotingTimeSeries = {
    VotingTimeSeries(jsonArr.value.toSeq.map(v => VoteDump.fromJsonObj(v.obj)).zipWithIndex.map { p =>
      IndexedVoteDump(p._1, p._2)
    })
  }
}
