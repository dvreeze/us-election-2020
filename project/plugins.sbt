
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.0.1")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.0.0")

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.7.0")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.3.2")

addSbtPlugin("io.github.cquiroz" % "sbt-tzdb" % "0.4.0")

libraryDependencies += "org.scala-js" %% "scalajs-env-jsdom-nodejs" % "1.0.0"
