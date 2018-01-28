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

  "org.scalatest" %% "scalatest" % "3.0.4" % "test"


)
