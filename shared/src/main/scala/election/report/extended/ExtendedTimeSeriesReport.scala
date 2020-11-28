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

package election.report.extended

import election.report.TimeSeriesReport
import ujson._

/**
 * Report, extended with more derived data.
 *
 * @author Chris de Vreeze
 */
final case class ExtendedTimeSeriesReport(reportEntries: Seq[ExtendedReportEntry]) {

  def report: TimeSeriesReport = {
    TimeSeriesReport(reportEntries.map(_.reportEntry))
  }

  def toJsonObj: Obj = {
    Obj(
      "timeseries" -> Arr(reportEntries.map(_.toJsonObj): _*),
    )
  }
}

object ExtendedTimeSeriesReport {

  def from(report: TimeSeriesReport): ExtendedTimeSeriesReport = {
    ExtendedTimeSeriesReport(report.reportEntries.map(e => ExtendedReportEntry.from(e)))
  }
}
