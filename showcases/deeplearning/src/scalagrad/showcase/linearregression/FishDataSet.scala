package scalagrad.showcase.linearregression

import scala.io.Source

object FishDataSet:

    case class Fish(
        species: String,
        weight: Double,
        length1: Double,
        length2: Double,
        length3: Double,
        height: Double,
        width: Double
    )

    def load: Seq[Fish] = 
        val content = Source.fromFile("showcases/deeplearning/res/fish/fish.csv")
            .getLines.map(_.split(","))
        val header = content.next
        val body = content.map(header.zip(_).toMap)

        body.map { row =>
            Fish(
                row(header(0)),  // row("Species"),
                row("Weight").toDouble,
                row("Length1").toDouble,
                row("Length2").toDouble,
                row("Length3").toDouble,
                row("Height").toDouble,
                row("Width").toDouble
            )
        }.toSeq