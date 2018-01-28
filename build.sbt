name := "chat-bot-server"

version := "0.1"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http" % "10.0.10",
  "com.typesafe.akka" %% "akka-http-spray-json" % "10.0.10",
  "io.spray" %%  "spray-json" % "1.3.3",


  "edu.stanford.nlp" % "stanford-corenlp" % "3.4",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.4" classifier "models",
  "edu.stanford.nlp" % "stanford-parser" % "3.4",

  "com.github.nikita-volkov" % "sext" % "0.2.4",

  "org.scalatest" %% "scalatest" % "3.0.4" % "test",
  "net.sf.jwordnet" % "jwnl"            % "1.4_rc3"
)

import sys.process._

unmanagedClasspath in Runtime += baseDirectory.value / "config"
unmanagedClasspath in Test    += baseDirectory.value / "config"
unmanagedClasspath in Compile += baseDirectory.value / "config"

lazy val `download-database` = taskKey[Unit]("Download the word-net database and installation to config and link")

// cf.https://stackoverflow.com/questions/27466869/download-a-zip-from-url-and-extract-it-in-resource-using-sbt
`download-database` := {
  val st        = streams.value
  val linkDir = file("link")
  val wnFile  = linkDir / "WordNet-3.0"
  if (wnFile.exists()) {
    st.log.info(s"WordNet installation ${wnFile.name} already present.")
  } else {
    st.log.info("Downloading WordNet...")
    IO.withTemporaryFile(prefix = "WordNet", postfix = "gz") { tmpFile =>
      (new URL("http://wordnetcode.princeton.edu/3.0/WordNet-3.0.tar.gz") #> tmpFile).!!
      Seq("tar", "-xf", tmpFile.getPath, "-C", linkDir.getPath).!!
    }
  }
}