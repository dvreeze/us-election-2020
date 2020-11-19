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

package election.queries

import election.data.Candidate
import election.data.VotingTimeSeries.IndexedVotingSnapshot
import election.queries.LostVotes.VoteLossData

/**
 * Lost votes computation. Taken from fraudcatch.py, found on site https://thedonald.win, and rewritten in Scala.
 *
 * @author Chris de Vreeze
 */
final case class LostVotes(candidate1: Candidate, candidate2: Candidate)
    extends ((IndexedVotingSnapshot, IndexedVotingSnapshot) => VoteLossData) {
  require(candidate1 != candidate2)

  import IndexedVotingSnapshot._

  def apply(snapshot1: IndexedVotingSnapshot, snapshot2: IndexedVotingSnapshot): VoteLossData = {
    requirePrecondition(snapshot1)
    requirePrecondition(snapshot2)

    var totalVotesLost: Long = 0L
    var totalVotesLostCandidate1: BigDecimal = 0
    var totalVotesLostCandidate2: BigDecimal = 0
    var totalVotesLostThirdParty: BigDecimal = 0

    if (voteShareChangedSubstantially(candidate1, snapshot1, snapshot2) && voteShareChangedSubstantially(candidate2, snapshot1, snapshot2)) {
      if (totalVotesDecreased(snapshot1, snapshot2) && votesOfCandidateDecreased(candidate1, snapshot1, snapshot2) && votesOfCandidateDecreased(
            candidate2,
            snapshot1,
            snapshot2)) {

        totalVotesLost = gainedTotalVotes(snapshot1, snapshot2)
        totalVotesLostCandidate1 = gainedVotesOfCandidateAsBigDecimal(candidate1, snapshot1, snapshot2)
        totalVotesLostCandidate2 = gainedVotesOfCandidateAsBigDecimal(candidate2, snapshot1, snapshot2)
        totalVotesLostThirdParty = totalVotesOfThirdPartyAsBigDecimal(snapshot2) - totalVotesOfThirdPartyAsBigDecimal(snapshot1)
      }
    }

    VoteLossData(totalVotesLostCandidate1, totalVotesLostCandidate2, totalVotesLostThirdParty, totalVotesLost)
  }

  /**
   * Requires that the vote shares are within bounds, and that vote totals are > 0. As a consequence, vote totals of candidates are > 0.
   */
  private def requirePrecondition(snapshot: IndexedVotingSnapshot): Unit = {
    require(snapshot.voteSharesAreWithinBounds, s"Vote shares not all within bounds (0 and 1)")
    require(snapshot.totalVotes > 0, s"Total votes not > 0")
  }

  private def voteShareChangedSubstantially(
      candidate: Candidate,
      snapshot1: IndexedVotingSnapshot,
      snapshot2: IndexedVotingSnapshot): Boolean = {
    require(snapshot1.isBefore(snapshot2), s"Snapshot $snapshot1 is not before $snapshot2")

    (snapshot2.voteShareOfCandidate(candidate) < snapshot1.voteShareOfCandidate(candidate) - margin) ||
    (snapshot2.voteShareOfCandidate(candidate) > snapshot1.voteShareOfCandidate(candidate) + margin)
  }

  private def totalVotesDecreased(snapshot1: IndexedVotingSnapshot, snapshot2: IndexedVotingSnapshot): Boolean = {
    require(snapshot1.isBefore(snapshot2), s"Snapshot $snapshot1 is not before $snapshot2")

    snapshot2.totalVotes < snapshot1.totalVotes
  }

  private def votesOfCandidateDecreased(
      candidate: Candidate,
      snapshot1: IndexedVotingSnapshot,
      snapshot2: IndexedVotingSnapshot): Boolean = {
    require(snapshot1.isBefore(snapshot2), s"Snapshot $snapshot1 is not before $snapshot2")

    snapshot2.totalVotesOfCandidateAsBigDecimal(candidate) < snapshot1.totalVotesOfCandidateAsBigDecimal(candidate)
  }

  private def voteShareOfThirdParty(snapshot: IndexedVotingSnapshot): BigDecimal = {
    val nonThirdPartyVoteShare: BigDecimal = snapshot.voteShares.view.filterKeys(Set(candidate1, candidate2)).values.sum
    BigDecimal(1) - nonThirdPartyVoteShare
  }

  private def totalVotesOfThirdPartyAsBigDecimal(snapshot: IndexedVotingSnapshot): BigDecimal = {
    voteShareOfThirdParty(snapshot) * snapshot.totalVotes
  }

  private val margin: BigDecimal = BigDecimal(0.001) // Exactly as in the Python script
}

object LostVotes {

  final case class VoteLossData(
      candidate1VoteLoss: BigDecimal,
      candidate2VoteLoss: BigDecimal,
      thirdPartyVoteLoss: BigDecimal,
      totalVotesLost: Long) {

    def plus(other: VoteLossData): VoteLossData = {
      VoteLossData(
        candidate1VoteLoss + other.candidate1VoteLoss,
        candidate2VoteLoss + other.candidate2VoteLoss,
        thirdPartyVoteLoss + other.thirdPartyVoteLoss,
        totalVotesLost + other.totalVotesLost
      )
    }
  }

  object VoteLossData {

    val empty: VoteLossData = VoteLossData(0L, 0L, 0L, 0L)
  }
}
