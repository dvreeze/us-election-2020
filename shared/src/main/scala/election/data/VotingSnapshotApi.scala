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

/**
 * Common API for voting snapshot data, with or without an array index in a voting time series.
 *
 * This trait is just there for convenience, making a voting snapshot with array index look the same as a voting snapshot without
 * the index, thus preventing the need for "unwrapping" the voting snapshot with index. It may be slight over-engineering,
 * but at the call site it makes things look a bit cleaner and less verbose.
 *
 * @author Chris de Vreeze
 */
trait VotingSnapshotApi {

  /**
   * Relative shares of the total votes per candidate, as a number between 0 and 1
   */
  def voteShares: Map[Candidate, BigDecimal]

  def voteShareOfCandidate(candidate: Candidate): BigDecimal

  /**
   * The total votes, for all candidates combined
   */
  def totalVotes: Long

  def timestamp: ZonedDateTime

  def otherData: Map[String, String]

  def totalVotesOfCandidate(candidate: Candidate): Long

  def totalVotesOfCandidateAsBigDecimal(candidate: Candidate): BigDecimal

  def containsAllOfCandidates(candidates: Set[Candidate]): Boolean

  /**
   * Returns true if all vote shares as well as the summarized vote shares are indeed numbers between 0 and 1
   */
  def voteSharesAreWithinBounds: Boolean

  def isBefore(other: VotingSnapshotApi): Boolean

  def isAfter(other: VotingSnapshotApi): Boolean

  /**
   * Returns true if the snapshot contains no votes, and can therefore be ignored
   */
  def isEmpty: Boolean

  /**
   * Returns true if method isEmpty returns false
   */
  def nonEmpty: Boolean
}
