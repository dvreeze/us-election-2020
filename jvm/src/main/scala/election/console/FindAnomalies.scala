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

package election.console

import java.io.File

import election.data.Candidate
import election.data.VotingSnapshot
import election.data.VotingTimeSeries
import election.data.VotingTimeSeries.IndexedVotingSnapshot
import election.queries.FindFraud
import election.queries.LostVotes
import ujson._

/**
 * Finds certain anomalies in the US 2020 election voting data set of a given state.
 *
 * @author Chris de Vreeze
 */
object FindAnomalies {

  def main(args: Array[String]): Unit = {
    require(args.sizeIs == 1 || args.sizeIs == 3, s"Usage: FindAnomalies <json data set of a state> [<candidate 1> <candidate 2>]")

    val jsonFile = new File(args(0)).ensuring(_.isFile)

    val candidate1: Candidate = if (args.sizeIs == 3) Candidate(args(1)) else Candidate.Trump
    val candidate2: Candidate = if (args.sizeIs == 3) Candidate(args(2)) else Candidate.Biden

    val allJsonData: Value = ujson.read(jsonFile)

    val timeseriesData: Arr = allJsonData("data")("races")(0)("timeseries").arr

    println(s"Number of (possibly empty) snapshots in the time series: ${timeseriesData.value.length}")

    println(
      s"Number of non-empty snapshots in the time series: ${timeseriesData.value.count(v => VotingSnapshot.fromJson(v.obj).nonEmpty)}")

    val timeSeries: VotingTimeSeries = VotingTimeSeries.fromJson(timeseriesData)

    require(timeSeries.isInChronologicalOrder, s"Not in chronological order")

    val expectedCandidates: Set[Candidate] = Set(candidate1, candidate2)

    require(
      timeSeries.snapshots.forall(_.containsAllOfCandidates(expectedCandidates)),
      s"Not every snapshot contains candidates ${expectedCandidates.mkString(", ")}"
    )

    require(timeSeries.nonEmptySnapshots.forall(_.totalVotes > 0L), s"Total votes not always > 0")
    require(timeSeries.nonEmptySnapshots.forall(_.totalVotesOfCandidate(candidate1) > 0L), s"$candidate1 votes not always > 0")
    require(timeSeries.nonEmptySnapshots.forall(_.totalVotesOfCandidate(candidate2) > 0L), s"$candidate2 votes not always > 0")

    require(timeSeries.snapshots.headOption.exists(_.isEmpty), s"Time series not starting with 'empty' snapshot")

    require(
      timeSeries.nonEmptySnapshots.forall(_.voteSharesAreWithinBounds),
      s"Vote shares not always within bounds (>= 0 and <= 1, also in total)")

    val snapshotPairsWithDisappearedVotes: Seq[(IndexedVotingSnapshot, IndexedVotingSnapshot)] =
      timeSeries.nonEmptySnapshotPairs.filter {
        case (snapshot1, snapshot2) =>
          snapshot1.totalVotes > snapshot2.totalVotes || {
            val candidates = snapshot1.voteShares.keySet
            candidates.exists(c => snapshot1.totalVotesOfCandidate(c) > snapshot2.totalVotesOfCandidate(c))
          }
      }

    println()
    println(
      s"Votes disappeared for candidate(s) and/or in total, whether switched or lost (${snapshotPairsWithDisappearedVotes.size} snapshot pairs):")

    snapshotPairsWithDisappearedVotes.foreach {
      case (snapshot1, snapshot2) =>
        println(s"Snapshots ${snapshot1.index} and ${snapshot2.index} (at ${snapshot1.timestamp} and ${snapshot2.timestamp})")
        println(
          s"\tDelta $candidate1 votes: ${snapshot2.totalVotesOfCandidate(candidate1) - snapshot1.totalVotesOfCandidate(candidate1)}" +
            s"  (from ${snapshot1.totalVotesOfCandidate(candidate1)} to ${snapshot2.totalVotesOfCandidate(candidate1)})")
        println(
          s"\tDelta $candidate2 votes: ${snapshot2.totalVotesOfCandidate(candidate2) - snapshot1.totalVotesOfCandidate(candidate2)}" +
            s"  (from ${snapshot1.totalVotesOfCandidate(candidate2)} to ${snapshot2.totalVotesOfCandidate(candidate2)})")
        println(
          s"\tDelta total votes: ${snapshot2.totalVotes - snapshot1.totalVotes}" +
            s"  (from ${snapshot1.totalVotes} to ${snapshot2.totalVotes})")
    }

    val lostVotes = LostVotes(candidate1, candidate2)

    val voteLoss: LostVotes.VoteLossData = timeSeries.nonEmptySnapshotPairs.foldLeft(LostVotes.VoteLossData.empty) {
      case (acc, (snapshot1, snapshot2)) =>
        acc.plus(lostVotes(snapshot1, snapshot2))
    }

    println()
    println("Vote loss, as per script fraudcatch.py (function lostvotes), found at https://thedonald.win")
    println(s"Total votes lost $candidate1: ${voteLoss.candidate1VoteLoss}")
    println(s"Total votes lost $candidate2: ${voteLoss.candidate2VoteLoss}")
    println(s"Total votes lost third: ${voteLoss.thirdPartyVoteLoss}")
    println(s"Total votes lost: ${voteLoss.totalVotesLost}")

    val findFraud: FindFraud = FindFraud(candidate1, candidate2)

    val fraudData: FindFraud.VoteSwapData = timeSeries.nonEmptySnapshotPairs.foldLeft(FindFraud.VoteSwapData.empty) {
      case (acc, (snapshot1, snapshot2)) =>
        acc.plus(findFraud(snapshot1, snapshot2))
    }

    println()
    println("Voting fraud, as per script fraudcatch.py (function findfraud), found at https://thedonald.win")

    println(s"$candidate1 lost: ${fraudData.totalVotesLostCandidate1}")
    println(s"$candidate1 lost to $candidate2: ${fraudData.candidate1ToCandidate2}")
    println(s"$candidate1 lost to third: ${fraudData.candidate1ToThird}")

    println(s"$candidate2 lost: ${fraudData.totalVotesLostCandidate2}")
    println(s"$candidate2 lost to $candidate1: ${fraudData.candidate2ToCandidate1}")
    println(s"$candidate2 lost to third: ${fraudData.candidate2ToThird}")

    println(s"Third lost: ${fraudData.totalVotesLostThirdParty}")
    println(s"Third lost to $candidate1: ${fraudData.thirdToCandidate1}")
    println(s"Third lost to $candidate2: ${fraudData.thirdToCandidate2}")

    val candidateProfitingMost = if (fraudData.candidate2ToCandidate1 > fraudData.candidate1ToCandidate2) candidate1 else candidate2
    val candidateProfitingLeast = if (candidateProfitingMost == candidate1) candidate2 else candidate1

    val netVotesSwitched = if (candidateProfitingLeast == candidate1) {
      fraudData.candidate1ToCandidate2 - fraudData.candidate2ToCandidate1
    } else {
      fraudData.candidate2ToCandidate1 - fraudData.candidate1ToCandidate2
    }

    println()
    println(s"Net votes switched from $candidateProfitingLeast to $candidateProfitingMost: $netVotesSwitched")

    println()
    println(s"Total votes lost (as per function findfraud): ${fraudData.totalVotesLost}")

    // Note: if we assumed bias against candidate 1 instead of candidate 2 (findFraud.swapBias) when the 3rd party loses votes, then the numbers
    // for votes lost by the 3rd party to candidates 1 and 2 would look different, of course.

    println()
    println(s"Ready")
  }
}
