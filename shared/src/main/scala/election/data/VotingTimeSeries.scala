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

import election.data.VotingTimeSeries.IndexedVotingSnapshot
import ujson._

/**
 * The voting time series
 *
 * @author Chris de Vreeze
 */
final case class VotingTimeSeries(snapshots: Seq[IndexedVotingSnapshot]) extends AnyVal {

  def nonEmptySnapshots: Seq[IndexedVotingSnapshot] = snapshots.filter(_.nonEmpty)

  def isInChronologicalOrder: Boolean = {
    nonEmptySnapshots.sliding(2).filter(_.sizeIs == 2).forall { xs =>
      xs.ensuring(_.sizeIs == 2)(0).isBefore(xs(1))
    }
  }

  def sortedChronologically: VotingTimeSeries = VotingTimeSeries(snapshots.sortBy(_.timestamp))

  def nonEmptySnapshotPairs: Seq[(IndexedVotingSnapshot, IndexedVotingSnapshot)] = {
    nonEmptySnapshots.sliding(2).filter(_.sizeIs == 2).map(xs => xs(0) -> xs(1)).toSeq
  }
}

object VotingTimeSeries {

  /**
   * VotingSnapshot with zero-based index in a VotingTimeSeries. Only non-empty ones should be counted.
   */
  final case class IndexedVotingSnapshot(snapshot: VotingSnapshot, index: Int) extends VotingSnapshotApi {

    def voteShares: Map[Candidate, BigDecimal] = snapshot.voteShares

    def voteShareOfCandidate(candidate: Candidate): BigDecimal = snapshot.voteShareOfCandidate(candidate)

    def totalVotes: Long = snapshot.totalVotes

    def timestamp: ZonedDateTime = snapshot.timestamp

    def otherData: Map[String, String] = snapshot.otherData

    def totalVotesOfCandidate(candidate: Candidate): Long = snapshot.totalVotesOfCandidate(candidate)

    def totalVotesOfCandidateAsBigDecimal(candidate: Candidate): BigDecimal = snapshot.totalVotesOfCandidateAsBigDecimal(candidate)

    def containsAllOfCandidates(candidates: Set[Candidate]): Boolean = snapshot.containsAllOfCandidates(candidates)

    def voteSharesAreWithinBounds: Boolean = snapshot.voteSharesAreWithinBounds

    def isBefore(other: VotingSnapshotApi): Boolean = snapshot.isBefore(other)

    def isAfter(other: VotingSnapshotApi): Boolean = snapshot.isAfter(other)

    def isEmpty: Boolean = snapshot.isEmpty

    def nonEmpty: Boolean = snapshot.nonEmpty
  }

  object IndexedVotingSnapshot {

    def gainedTotalVotes(snapshot1: IndexedVotingSnapshot, snapshot2: IndexedVotingSnapshot): Long = {
      VotingSnapshot.gainedTotalVotes(snapshot1.snapshot, snapshot2.snapshot)
    }

    def gainedVotesOfCandidate(candidate: Candidate, snapshot1: IndexedVotingSnapshot, snapshot2: IndexedVotingSnapshot): Long = {
      VotingSnapshot.gainedVotesOfCandidate(candidate, snapshot1.snapshot, snapshot2.snapshot)
    }

    def gainedVotesOfCandidateAsBigDecimal(
        candidate: Candidate,
        snapshot1: IndexedVotingSnapshot,
        snapshot2: IndexedVotingSnapshot): BigDecimal = {
      VotingSnapshot.gainedVotesOfCandidateAsBigDecimal(candidate, snapshot1.snapshot, snapshot2.snapshot)
    }

    def gainedVoteShareOfCandidate(candidate: Candidate, snapshot1: IndexedVotingSnapshot, snapshot2: IndexedVotingSnapshot): BigDecimal = {
      VotingSnapshot.gainedVoteShareOfCandidate(candidate, snapshot1.snapshot, snapshot2.snapshot)
    }
  }

  def fromJson(jsonArr: Arr): VotingTimeSeries = {
    VotingTimeSeries(jsonArr.value.toSeq.map(v => VotingSnapshot.fromJson(v.obj)).zipWithIndex.map { p =>
      IndexedVotingSnapshot(p._1, p._2)
    })
  }
}
