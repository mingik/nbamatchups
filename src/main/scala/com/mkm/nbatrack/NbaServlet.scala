package com.mkm.nbatrack

import org.scalatra._
import org.scalatra.json._

import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods.{parse, compact}

import java.time.Instant
import java.nio.charset.StandardCharsets

import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source

class NbaServlet extends NbatrackStack with FutureSupport {

  protected implicit def executor = ExecutionContext.global

  get("/") {
    <html>
      <body>
        <h1>Hello, world!</h1>
        Say <a href="hello-scalate">hello to Scalate</a>.

        Main page will contain two submit forms for two teams.
      </body>
    </html>
  }

  get("/api/v1/team/:team") {
    val teamName = params("team")

    Teams.teamId(teamName) match {
      case Some(teamId) => compact(("result" -> StatsNBA.team(teamId)) ~ ("time" -> Instant.now.toString))
      case None => JNothing
    }
      
  }

  post("/api/v1/matchup") {
    // TODO: add pattern-match check agains a list of allowed teams
    val team1 = params("team1")
    val team2 = params("team2")

    val teams: Option[Pair[Team, Team]] = for {
      team1Id <- Teams.teamId(team1)
      team2Id <- Teams.teamId(team2)
    } yield (Team(team1, team2Id), Team(team2, team2Id))


    val teamWonName: Option[String] = for {
      teamPair <- teams
      teamWon <- Matchup.result(teamPair)
    } yield teamWon.name
    
    compact(("result" -> teamWonName.getOrElse("None")) ~ ("time" -> Instant.now.toString))
  }

  post("/api/v1/tournament") {
    val teams: List[Team] = for {
      team <- multiParams("team").toList
      teamId <- Teams.teamId(team)
    } yield Team(team, teamId)

    compact(("result" -> Tournament.winner(teams).map(_.name).getOrElse("None")) ~ ("time" -> Instant.now.toString))
  }

}

object Matchup {
  def result(pair: Pair[Team, Team]): Option[Team] = {
    // 1) Query stats.nba.com in order to get information about two teams
    // 2) Perform analytics to determine the result
    Some(pair._1)
  }
}

object Tournament {
  def winner(teams: List[Team]): Option[Team] = {
    // 1) Query stats.nba.com in order to get information about all teams
    // 2) Perform anaylitcs to determine the winner of the tournament
    teams match {
      case firstTeam :: rest => Some(firstTeam)
      case _ => None
    }
  }
}

case class Team(val name: String, val teamId: String)

object Teams {
  val nameToId = Map(
    "AtlantaHawks"          -> "1610612737",
    "BostonCeltics"         -> "1610612738",
    "BrooklynNets"          -> "1610612751",
    "CharlotteHornets"      -> "1610612766",
    "ChicagoBulls"          -> "1610612741",
    "ClevelandCavaliers"    -> "1610612739",
    "DallasMavericks"       -> "1610612742",
    "DenverNuggets"         -> "1610612743",
    "DetroitPistons"        -> "1610612765",
    "GoldenStateWarriors"   -> "1610612744",
    "Houston Rockets"       -> "1610612745",
    "IndianaPacers"         -> "1610612754",
    "LosAngelesClippers"    -> "1610612746",
    "LosAngelesLakers"      -> "1610612747",
    "MemphisGrizzlies"      -> "1610612763",
    "MiamiHeat"             -> "1610612748",
    "MilwaukeeBucks"        -> "1610612749",
    "MinnesotaTimberwolves" -> "1610612750",
    "NewOrleansPelicans"    -> "1610612740",
    "NewYorkKnicks"         -> "1610612752",
    "OklahomaCityThunder"   -> "1610612760",
    "OrlandoMagic"          -> "1610612753",
    "Philadelphia76ers"     -> "1610612755",
    "PhoenixSuns"           -> "1610612756",
    "PortlandTrailBlazers"  -> "1610612757",
    "SacramentoKings"       -> "1610612758",
    "SanAntonioSpurs"       -> "1610612759",
    "TorontoRaptors"        -> "1610612761",
    "UtahJazz"              -> "1610612762",
    "WashingtonWizards"     -> "1610612764"
  )

  def teamId(teamName: String): Option[String] = nameToId.get(teamName)
}


object StatsNBA {
  val statsNbaURL = "http://stats.nba.com/stats/"
  val teamRosterURI = "commonteamroster/?"
  val currentSeasonURI = "&Season=2015-16"
  def teamIDURI(teamId: String) = s"TeamID=$teamId"
  /*
   * This method extracts team's infor from stats.nba.com and outputs JSON
   */
  def team(teamId: String): JValue = {
    //?TeamID=1610612737&Season=1995-96
    val teamStatsNBAURL = statsNbaURL + teamRosterURI + teamIDURI(teamId) + currentSeasonURI
    println(s"Calling URL: $teamStatsNBAURL")
    parse(Source.fromURL(teamStatsNBAURL, StandardCharsets.UTF_8.name()).mkString)
  }
}
