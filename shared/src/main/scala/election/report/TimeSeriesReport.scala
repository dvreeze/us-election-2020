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

import election.data.Candidate
import election.data.VotingTimeSeries
import ujson._

/**
 * Report, corresponding to a voting time series.
 *
 * The report immediately gives more information to the reader than the original voting time series.
 *
 * @author Chris de Vreeze
 */
final case class TimeSeriesReport(reportEntries: Seq[ReportEntry]) {

  /**
   * Returns true if all report entries are in chronological order. If 2 subsequent entries have the same timestamp,
   * they are still considered to be in chronological order. The elapsed seconds of the first report entry are ignored, however.
   */
  def isInChronologicalOrder: Boolean = {
    reportEntries.drop(1).forall(_.deltaSeconds >= 0)
  }

  def toJsonObj: Obj = {
    Obj("timeseries" -> Arr(reportEntries.map(_.toJsonObj): _*))
  }

  /*
   * Sorts the entries by the given sort criteria. Of course this breaks the chronological order.
   */
  def sortBy(f: ReportEntry => BigDecimal): TimeSeriesReport = {
    TimeSeriesReport(reportEntries.sortBy(f))
  }

  /*
   * Sorts the entries by the given sort criteria, yet in descending order. Of course this breaks the chronological order.
   */
  def sortByDesc(f: ReportEntry => BigDecimal): TimeSeriesReport = {
    TimeSeriesReport(reportEntries.sortBy(f).reverse)
  }
}

object TimeSeriesReport {

  def from(votingTimeSeries: VotingTimeSeries, candidate1: Candidate, candidate2: Candidate): TimeSeriesReport = {
    require(votingTimeSeries.snapshots.nonEmpty, s"Missing voting snapshots")
    require(votingTimeSeries.snapshots.head.isEmpty, s"First voting snapshot not empty (all zeroes), which it must be")

    val reportEntries: Seq[ReportEntry] = votingTimeSeries.snapshotPairs.map {
      case (prevSnapshot, snapshot) =>
        ReportEntry.from(prevSnapshot, snapshot, candidate1, candidate2)
    }

    TimeSeriesReport(reportEntries)
  }

  def fromJsonObj(jsonObj: Obj): TimeSeriesReport = {
    require(jsonObj.value.keySet.contains("timeseries"), s"Missing key 'timeseries'")

    fromJsonArr(jsonObj("timeseries").arr)
  }

  def fromJsonArr(jsonArr: Arr): TimeSeriesReport = {
    val reportEntries: Seq[ReportEntry] = jsonArr.value.toSeq.map(v => ReportEntry.fromJsonObj(v.obj))
    TimeSeriesReport(reportEntries)
  }
}
