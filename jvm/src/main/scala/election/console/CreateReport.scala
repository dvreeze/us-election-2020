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
import election.data.VotingTimeSeries
import election.report.TimeSeriesReport
import ujson._

/**
 * Creates a JSON report of the time series in the US 2020 election voting data set of a given state. The report is more informative
 * to a reader than the input JSON, because voting totals (per candidate) are shown, as well as deltas compared to the preceding
 * voting shapshots.
 *
 * @author Chris de Vreeze
 */
object CreateReport {

  def main(args: Array[String]): Unit = {
    require(args.sizeIs == 1 || args.sizeIs == 3, s"Usage: CreateReport <json data set of a state> [<candidate 1> <candidate 2>]")

    val jsonFile = new File(args(0)).ensuring(_.isFile)

    val candidate1: Candidate = if (args.sizeIs == 3) Candidate(args(1)) else Candidate.Trump
    val candidate2: Candidate = if (args.sizeIs == 3) Candidate(args(2)) else Candidate.Biden

    val report: TimeSeriesReport = createReport(jsonFile, candidate1, candidate2)

    val reportJson: Obj = report.toJson

    println(write(reportJson, indent = 2))
  }

  def createReport(jsonInputFile: File, candidate1: Candidate, candidate2: Candidate): TimeSeriesReport = {
    val allJsonData: Value = ujson.read(jsonInputFile)

    val timeseriesData: Arr = allJsonData("data")("races")(0)("timeseries").arr

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

    val report: TimeSeriesReport = TimeSeriesReport.from(timeSeries, candidate1, candidate2)
    report
  }
}
