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
import election.data.VotingTimeSeries.IndexedVoteDump
import election.queries.LostVotes.VoteLossData

/**
 * Lost votes computation. Taken from fraudcatch.py, found on site https://thedonald.win, and rewritten in Scala.
 *
 * @author Chris de Vreeze
 */
final case class LostVotes(candidate1: Candidate, candidate2: Candidate) extends ((IndexedVoteDump, IndexedVoteDump) => VoteLossData) {
  require(candidate1 != candidate2)

  import IndexedVoteDump._

  def apply(voteDump1: IndexedVoteDump, voteDump2: IndexedVoteDump): VoteLossData = {
    requirePrecondition(voteDump1)
    requirePrecondition(voteDump2)

    var totalVotesLost: Long = 0L
    var totalVotesLostCandidate1: BigDecimal = 0
    var totalVotesLostCandidate2: BigDecimal = 0
    var totalVotesLostThirdParty: BigDecimal = 0

    if (voteShareChangedSubstantially(candidate1, voteDump1, voteDump2) && voteShareChangedSubstantially(candidate2, voteDump1, voteDump2)) {
      if (totalVotesDecreased(voteDump1, voteDump2) && votesOfCandidateDecreased(candidate1, voteDump1, voteDump2) && votesOfCandidateDecreased(
            candidate2,
            voteDump1,
            voteDump2)) {

        totalVotesLost = gainedTotalVotes(voteDump1, voteDump2)
        totalVotesLostCandidate1 = gainedVotesOfCandidateAsBigDecimal(candidate1, voteDump1, voteDump2)
        totalVotesLostCandidate2 = gainedVotesOfCandidateAsBigDecimal(candidate2, voteDump1, voteDump2)
        totalVotesLostThirdParty = totalVotesOfThirdPartyAsBigDecimal(voteDump2) - totalVotesOfThirdPartyAsBigDecimal(voteDump1)
      }
    }

    VoteLossData(totalVotesLostCandidate1, totalVotesLostCandidate2, totalVotesLostThirdParty, totalVotesLost)
  }

  /**
   * Requires that the vote shares are within bounds, and that vote totals are > 0. As a consequence, vote totals of candidates are > 0.
   */
  private def requirePrecondition(voteDump: IndexedVoteDump): Unit = {
    require(voteDump.voteSharesAreWithinBounds, s"Vote shares not all within bounds (0 and 1)")
    require(voteDump.totalVotes > 0, s"Total votes not > 0")
  }

  private def voteShareChangedSubstantially(candidate: Candidate, voteDump1: IndexedVoteDump, voteDump2: IndexedVoteDump): Boolean = {
    require(!voteDump1.isAfter(voteDump2), s"Vote dump $voteDump1 is after $voteDump2")

    (voteDump2.voteShareOfCandidate(candidate) < voteDump1.voteShareOfCandidate(candidate) - margin) ||
    (voteDump2.voteShareOfCandidate(candidate) > voteDump1.voteShareOfCandidate(candidate) + margin)
  }

  private def totalVotesDecreased(voteDump1: IndexedVoteDump, voteDump2: IndexedVoteDump): Boolean = {
    require(!voteDump1.isAfter(voteDump2), s"Vote dump $voteDump1 is after $voteDump2")

    voteDump2.totalVotes < voteDump1.totalVotes
  }

  private def votesOfCandidateDecreased(candidate: Candidate, voteDump1: IndexedVoteDump, voteDump2: IndexedVoteDump): Boolean = {
    require(!voteDump1.isAfter(voteDump2), s"Vote dump $voteDump1 is after $voteDump2")

    voteDump2.totalVotesOfCandidateAsBigDecimal(candidate) < voteDump1.totalVotesOfCandidateAsBigDecimal(candidate)
  }

  private def voteShareOfThirdParty(voteDump: IndexedVoteDump): BigDecimal = {
    val nonThirdPartyVoteShare: BigDecimal = voteDump.voteShares.view.filterKeys(Set(candidate1, candidate2)).values.sum
    BigDecimal(1) - nonThirdPartyVoteShare
  }

  private def totalVotesOfThirdPartyAsBigDecimal(voteDump: IndexedVoteDump): BigDecimal = {
    voteShareOfThirdParty(voteDump) * voteDump.totalVotes
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
