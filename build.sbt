ThisBuild / version := "0.0.1"
ThisBuild / scalaVersion := "3.2.2"
ThisBuild / organization := "ch.benikm91"

lazy val spireDependency = Seq(
    libraryDependencies += "org.typelevel" %% "spire" % "0.18.0"
)

lazy val breezeDependency = Seq(
  libraryDependencies ++= Seq(
    "org.scalanlp" %% "breeze" % "2.1.0",
  )
)

lazy val basicSettings = Seq(
    Compile / scalaSource := baseDirectory.value / "src",
    Compile / resourceDirectory := baseDirectory.value / "res",
    Test / scalaSource := baseDirectory.value / "test",
    Test / parallelExecution := true,
)

lazy val scalaTestSettings = Seq(
  libraryDependencies ++= Seq(
    "org.scalactic" %% "scalactic" % "3.2.14" % Test, 
    "org.scalatest" %% "scalatest" % "3.2.14" % Test,
    "org.scalatestplus" %% "scalacheck-1-17" % "3.2.14.0" % Test
  )
)

// ScalaGrad API
lazy val scalaGradApi = (project in file("./scala-grad-api"))
  .settings(
    name := "scala-grad-api",
    basicSettings,
    spireDependency,
    breezeDependency,
    scalaTestSettings,
  )

// ScalaGrad Breeze
lazy val scalaGradAutoBreezeApi = (project in file("./scala-grad-auto-breeze"))
  .settings(
    name := "scala-grad-auto-breeze",
    basicSettings,
    breezeDependency,
  ).dependsOn(
    scalaGradApi,
    scalaGradApi % "test->test",
  )

// ScalaGrad Breeze
lazy val deepLearningShowcase = (project in file("./showcases/deeplearning"))
  .settings(
    name := "scala-grad-showcase-deeplearning",
    basicSettings,
    breezeDependency,
  ).dependsOn(
    scalaGradAutoBreezeApi
  )

lazy val root = (project in file("."))
  .settings(
    name := "scala-grad",
  )
  .aggregate(
    // API
    scalaGradApi,
    // Breeze
    scalaGradAutoBreezeApi,
    // Showcase
    deepLearningShowcase,
  )
  .enablePlugins(MdocPlugin)