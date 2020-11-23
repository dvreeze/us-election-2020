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
import java.io.FileWriter

import scala.util.Try
import scala.util.Using

import ujson._

/**
 * Prettifies the time series JSON (or any JSON for that matter) in the US 2020 election voting data set (of 1 or more states).
 *
 * Program arguments: the input JSON (or directory), output directory.
 *
 * @author Chris de Vreeze
 */
object PrettifyJson {

  def main(args: Array[String]): Unit = {
    require(args.sizeIs == 2, s"Usage: PrettifyJson <json data set> <output dir>")

    val jsonFileOrDir: File = new File(args(0))

    val outputDir: File = new File(args(1))
    outputDir.mkdirs()
    require(outputDir.isDirectory, s"Not a directory: $outputDir")

    val jsonFiles: Seq[File] = if (jsonFileOrDir.isFile) {
      Seq(jsonFileOrDir)
    } else {
      require(jsonFileOrDir.isDirectory, s"Not a directory: $jsonFileOrDir")

      jsonFileOrDir
        .listFiles(f => f.isFile && f.getName.endsWith(".json"))
        .toSeq
        .sortBy(_.getName)
    }

    jsonFiles.foreach { f =>
      println(s"Prettifying file '$f'")

      Try {
        val allJsonData: Value = ujson.read(f)

        Using.resource(new FileWriter(new File(outputDir, f.getName))) { fw =>
          writeTo(allJsonData, fw, indent = 2)
        }
      }.recover { case t: Exception => println(s"Exception thrown: $t") }
    }
  }
}
