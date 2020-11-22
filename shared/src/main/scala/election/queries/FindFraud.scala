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
import election.queries.FindFraud.VoteSwapData

/**
 * Find fraud computation. Taken from fraudcatch.py, found on site https://thedonald.win, and rewritten in Scala.
 *
 * @author Chris de Vreeze
 */
final case class FindFraud(candidate1: Candidate, candidate2: Candidate, biasedAgainstCandidate1: Boolean)
    extends ((IndexedVotingSnapshot, IndexedVotingSnapshot) => VoteSwapData) {
  require(candidate1 != candidate2)

  /**
   * Swaps the candidate against we are biased when 3rd party votes get lost.
   */
  def swapBias: FindFraud = FindFraud(candidate1, candidate2, !biasedAgainstCandidate1)

  /**
   * Having bias against candidate 1 when 3rd party votes get lost.
   */
  def havingBiasAgainstCandidate1: FindFraud = FindFraud(candidate1, candidate2, biasedAgainstCandidate1 = true)

  /**
   * Having bias against candidate 2 when 3rd party votes get lost.
   */
  def havingBiasAgainstCandidate2: FindFraud = FindFraud(candidate1, candidate2, biasedAgainstCandidate1 = false)

  def apply(snapshot1: IndexedVotingSnapshot, snapshot2: IndexedVotingSnapshot): VoteSwapData = {
    requirePrecondition(snapshot1)
    requirePrecondition(snapshot2)

    voteSwaps(candidate1, candidate2, snapshot1, snapshot2)
      .plus(voteSwaps(candidate2, candidate1, snapshot1, snapshot2))
      .plus(voteSwapsOfThirdParty(snapshot1, snapshot2))
  }

  /**
   * Requires that the vote shares are within bounds, and that vote totals are > 0. As a consequence, vote totals of candidates are > 0.
   */
  private def requirePrecondition(snapshot: IndexedVotingSnapshot): Unit = {
    require(snapshot.voteSharesAreWithinBounds, s"Vote shares not all within bounds (0 and 1)")
    require(snapshot.totalVotes > 0, s"Total votes not > 0")
  }

  private def voteSwaps(
      candidate: Candidate,
      otherCandidate: Candidate,
      snapshot1: IndexedVotingSnapshot,
      snapshot2: IndexedVotingSnapshot): VoteSwapData = {
    require(!snapshot1.isAfter(snapshot2), s"Snapshot $snapshot1 is after $snapshot2")
    require(Set(candidate, otherCandidate) == Set(candidate1, candidate2), s"Missing candidate 1 and/or 2")

    val candidateVotesNow: BigDecimal = snapshot2.totalVotesOfCandidateAsBigDecimal(candidate)
    val candidateVotesThen: BigDecimal = snapshot1.totalVotesOfCandidateAsBigDecimal(candidate)

    val otherCandidateVotesNow: BigDecimal = snapshot2.totalVotesOfCandidateAsBigDecimal(otherCandidate)
    val otherCandidateVotesThen: BigDecimal = snapshot1.totalVotesOfCandidateAsBigDecimal(otherCandidate)

    val thirdPartyVotesNow: BigDecimal = totalVotesOfThirdPartyAsBigDecimal(snapshot2)
    val thirdPartyVotesThen: BigDecimal = totalVotesOfThirdPartyAsBigDecimal(snapshot1)

    var voteSwaps: VoteSwapData = VoteSwapData.empty

    val candidateIsCandidate1: Boolean = candidate == candidate1

    if (candidateVotesNow < candidateVotesThen && (candidateVotesThen - candidateVotesNow) > margin(snapshot2)) {
      // The candidate lost votes compared to the preceding snapshot, and lost even more of them than the computed margin

      if (otherCandidateVotesNow > otherCandidateVotesThen || thirdPartyVotesNow > thirdPartyVotesThen) {
        // Yet the other candidate and/or the third party did not lose any votes

        if ((candidateVotesNow - candidateVotesThen <= otherCandidateVotesNow - otherCandidateVotesThen) ||
            (candidateVotesNow - candidateVotesThen <= thirdPartyVotesNow - thirdPartyVotesThen)) {
          // The candidate lost the most votes

          var remainingCandidateLostTotal: BigDecimal = candidateVotesThen - candidateVotesNow

          // Below the algorithm is biased, because the other candidate is processed before the 3rd party.

          if (otherCandidateVotesNow > otherCandidateVotesThen) {
            assert(candidateVotesNow - candidateVotesThen <= otherCandidateVotesNow - otherCandidateVotesThen) // Part of if-condition in Python script

            if (otherCandidateVotesNow - otherCandidateVotesThen > remainingCandidateLostTotal) {
              voteSwaps = voteSwaps.addCandidateToOtherCandidate(remainingCandidateLostTotal, candidateIsCandidate1)
              remainingCandidateLostTotal = 0
            } else {
              voteSwaps = voteSwaps.addCandidateToOtherCandidate(otherCandidateVotesNow - otherCandidateVotesThen, candidateIsCandidate1)
              remainingCandidateLostTotal -= otherCandidateVotesNow - otherCandidateVotesThen
            }
          }

          if (thirdPartyVotesNow > thirdPartyVotesThen) {
            assert(candidateVotesNow - candidateVotesThen <= thirdPartyVotesNow - thirdPartyVotesThen) // Part of if-condition in Python script

            if (thirdPartyVotesNow - thirdPartyVotesThen > remainingCandidateLostTotal) {
              voteSwaps = voteSwaps.addCandidateToThird(remainingCandidateLostTotal, candidateIsCandidate1)
              remainingCandidateLostTotal = 0
            } else {
              voteSwaps = voteSwaps.addCandidateToThird(thirdPartyVotesNow - thirdPartyVotesThen, candidateIsCandidate1)
              remainingCandidateLostTotal -= thirdPartyVotesNow - thirdPartyVotesThen
            }
          }
        }
      }
    }

    voteSwaps
  }

  private def voteSwapsOfThirdParty(snapshot1: IndexedVotingSnapshot, snapshot2: IndexedVotingSnapshot): VoteSwapData = {
    require(!snapshot1.isAfter(snapshot2), s"Snapshot $snapshot1 is after $snapshot2")

    val candidate: Candidate = if (biasedAgainstCandidate1) candidate1 else candidate2
    val otherCandidate: Candidate = if (biasedAgainstCandidate1) candidate2 else candidate1

    val candidateVotesNow: BigDecimal = snapshot2.totalVotesOfCandidateAsBigDecimal(candidate)
    val candidateVotesThen: BigDecimal = snapshot1.totalVotesOfCandidateAsBigDecimal(candidate)

    val otherCandidateVotesNow: BigDecimal = snapshot2.totalVotesOfCandidateAsBigDecimal(otherCandidate)
    val otherCandidateVotesThen: BigDecimal = snapshot1.totalVotesOfCandidateAsBigDecimal(otherCandidate)

    val thirdPartyVotesNow: BigDecimal = totalVotesOfThirdPartyAsBigDecimal(snapshot2)
    val thirdPartyVotesThen: BigDecimal = totalVotesOfThirdPartyAsBigDecimal(snapshot1)

    var voteSwaps: VoteSwapData = VoteSwapData.empty

    if (thirdPartyVotesNow < thirdPartyVotesThen && (thirdPartyVotesThen - thirdPartyVotesNow) > margin(snapshot2)) {
      // The third party lost votes compared to the preceding snapshot, and lost even more of them than the computed margin

      // Here the Python script is different: thirdPartyVotesNow < thirdPartyVotesThen. Is that a bug in that script, or in this program?
      if (candidateVotesNow > candidateVotesThen || otherCandidateVotesNow > otherCandidateVotesThen) {
        // Yet candidate 1 or candidate 2 did not lose any votes

        if ((thirdPartyVotesNow - thirdPartyVotesThen <= candidateVotesNow - candidateVotesThen) ||
            (thirdPartyVotesNow - thirdPartyVotesThen <= otherCandidateVotesNow - otherCandidateVotesThen)) {
          // The third party lost the most votes

          var remainingThirdLostTotal: BigDecimal = thirdPartyVotesThen - thirdPartyVotesNow

          // Below the algorithm is biased, but we can change the candidate we are biased against.

          if (candidateVotesNow > candidateVotesThen) {
            assert(thirdPartyVotesNow - thirdPartyVotesThen <= candidateVotesNow - candidateVotesThen) // Part of if-condition in Python script

            if (candidateVotesNow - candidateVotesThen > remainingThirdLostTotal) {
              voteSwaps = voteSwaps.addThirdToCandidate(remainingThirdLostTotal, candidate == candidate1)
              remainingThirdLostTotal = 0
            } else {
              voteSwaps = voteSwaps.addThirdToCandidate(candidateVotesNow - candidateVotesThen, candidate == candidate1)
              remainingThirdLostTotal -= candidateVotesNow - candidateVotesThen
            }
          }

          if (otherCandidateVotesNow > otherCandidateVotesThen) {
            assert(thirdPartyVotesNow - thirdPartyVotesThen <= otherCandidateVotesNow - otherCandidateVotesThen) // Part of if-condition in Python script

            if (otherCandidateVotesNow - otherCandidateVotesThen > remainingThirdLostTotal) {
              voteSwaps = voteSwaps.addThirdToCandidate(remainingThirdLostTotal, otherCandidate == candidate1)
              remainingThirdLostTotal = 0
            } else {
              voteSwaps = voteSwaps.addThirdToCandidate(otherCandidateVotesNow - otherCandidateVotesThen, otherCandidate == candidate1)
              remainingThirdLostTotal -= otherCandidateVotesNow - otherCandidateVotesThen
            }
          }
        }
      }
    }

    voteSwaps
  }

  private def voteShareOfThirdParty(snapshot: IndexedVotingSnapshot): BigDecimal = {
    val nonThirdPartyVoteShare: BigDecimal = snapshot.voteShares.view.filterKeys(Set(candidate1, candidate2)).values.sum
    BigDecimal(1) - nonThirdPartyVoteShare
  }

  private def totalVotesOfThirdPartyAsBigDecimal(snapshot: IndexedVotingSnapshot): BigDecimal = {
    voteShareOfThirdParty(snapshot) * snapshot.totalVotes
  }

  private def margin(snapshot: IndexedVotingSnapshot): BigDecimal = {
    (BigDecimal(0.00049999) * snapshot.totalVotes) + 50 // Exactly as in the Python script
  }
}

object FindFraud {

  def apply(candidate1: Candidate, candidate2: Candidate): FindFraud = FindFraud(candidate1, candidate2, biasedAgainstCandidate1 = false)

  final case class VoteSwapData(
      candidate1ToCandidate2: BigDecimal,
      candidate1ToThird: BigDecimal,
      candidate2ToCandidate1: BigDecimal,
      candidate2ToThird: BigDecimal,
      thirdToCandidate1: BigDecimal,
      thirdToCandidate2: BigDecimal) {

    def totalVotesLostCandidate1: BigDecimal = candidate1ToCandidate2 + candidate1ToThird

    def totalVotesLostCandidate2: BigDecimal = candidate2ToCandidate1 + candidate2ToThird

    def totalVotesLostThirdParty: BigDecimal = thirdToCandidate1 + thirdToCandidate2

    def totalVotesLost: BigDecimal = {
      totalVotesLostCandidate1 + totalVotesLostCandidate2 + totalVotesLostThirdParty
    }

    def plus(other: VoteSwapData): VoteSwapData = {
      VoteSwapData(
        candidate1ToCandidate2 + other.candidate1ToCandidate2,
        candidate1ToThird + other.candidate1ToThird,
        candidate2ToCandidate1 + other.candidate2ToCandidate1,
        candidate2ToThird + other.candidate2ToThird,
        thirdToCandidate1 + other.thirdToCandidate1,
        thirdToCandidate2 + other.thirdToCandidate2
      )
    }

    def addCandidateToOtherCandidate(votes: BigDecimal, candidateIsCandidate1: Boolean): VoteSwapData = {
      if (candidateIsCandidate1) {
        addCandidate1ToCandidate2(votes)
      } else {
        addCandidate2ToCandidate1(votes)
      }
    }

    def addCandidate1ToCandidate2(votes: BigDecimal): VoteSwapData = {
      this.copy(candidate1ToCandidate2 = this.candidate1ToCandidate2 + votes)
    }

    def addCandidate2ToCandidate1(votes: BigDecimal): VoteSwapData = {
      this.copy(candidate2ToCandidate1 = this.candidate2ToCandidate1 + votes)
    }

    def addCandidateToThird(votes: BigDecimal, candidateIsCandidate1: Boolean): VoteSwapData = {
      if (candidateIsCandidate1) {
        addCandidate1ToThird(votes)
      } else {
        addCandidate2ToThird(votes)
      }
    }

    def addCandidate1ToThird(votes: BigDecimal): VoteSwapData = {
      this.copy(candidate1ToThird = this.candidate1ToThird + votes)
    }

    def addCandidate2ToThird(votes: BigDecimal): VoteSwapData = {
      this.copy(candidate2ToThird = this.candidate2ToThird + votes)
    }

    def addThirdToCandidate(votes: BigDecimal, candidateIsCandidate1: Boolean): VoteSwapData = {
      if (candidateIsCandidate1) {
        addThirdToCandidate1(votes)
      } else {
        addThirdToCandidate2(votes)
      }
    }

    def addThirdToCandidate1(votes: BigDecimal): VoteSwapData = {
      this.copy(thirdToCandidate1 = this.thirdToCandidate1 + votes)
    }

    def addThirdToCandidate2(votes: BigDecimal): VoteSwapData = {
      this.copy(thirdToCandidate2 = this.thirdToCandidate2 + votes)
    }
  }

  object VoteSwapData {

    val empty: VoteSwapData = VoteSwapData(0L, 0L, 0L, 0L, 0L, 0L)
  }
}
