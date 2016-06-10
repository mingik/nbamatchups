// scala console from project

import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import scala.io._

val allPlayersURL = "http://stats.nba.com/stats/commonallplayers?LeagueID=00&Season=2015-16&IsOnlyCurrentSeason=1"

val allPlayersResponse = Source.fromURL(allPlayersURL)

val allPlayersJS = parse( allPlayersResponse.mkString )

val playersJS = allPlayers \ "resultSets" \ "rowSet"

val playerJSStrip = playersJS.children.map( en => List(en(2),en(0)))

implicit val formats = org.json4s.DefaultFormats

val playersInfo = playerJSStrip.map( li => (li(0).extract[String], li(1).extract[Int]))

val playersNameId = playersInfo.map( en => (en._1.replace(" ",""), en._2))

import java.io._

val pw = new PrintWriter(new File("players.txt"))


