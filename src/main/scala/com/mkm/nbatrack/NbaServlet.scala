package com.mkm.nbatrack

import org.scalatra._
import org.scalatra.json._

import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods.{parse, compact}

import java.time.Instant
import java.time.temporal.ChronoUnit
import java.nio.charset.StandardCharsets

import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source

class NbaServlet extends NbatrackStack with FutureSupport {

  protected implicit def executor = ExecutionContext.global

  get("/") {
    <html>
      <body>
        <h1>Hello, world!</h1>
        Greet lucky <a href="/greet/me">me</a>.

        Main page will contain two submit forms for two teams.
      </body>
    </html>
  }

  get("/greet/:whom") {
    val lucky =
      for (i <- (1 to 8).toList) yield scala.util.Random.nextInt(48) + 1
    layoutTemplate("greeter.html", "whom" -> params("whom"), "lucky" -> lucky)
  }

/////////////////////// Info ///////////////////////////////

  get("/api/v1/team/:team") {
    val teamName = params("team")

    StatsNBA.teamByName(teamName) match {
      case JNothing => compact(JNothing)
      case jValue: JValue => compact(jValue)
    }
      
  }

  get("/api/v1/player/:player") {
    val playerName = params("player")

    StatsNBA.playerByName(playerName) match {
      case JNothing => compact(JNothing)
      case jValue: JValue => compact(jValue)
    }
      
  }

//////////////////////// Matchup ////////////////////////////////

  post("/api/v1/team/matchup") {

    val team1Name = params("team1")
    val team2Name = params("team2")

    val teamWonJValue = for {
      team1Id <- StatsNBA.teamId(team1Name)
      team2Id <- StatsNBA.teamId(team2Name)
    } yield TeamMatchup.result((Team(team1Name, team2Id), Team(team2Name, team2Id)))

    teamWonJValue match {
      case Some(jValue) => compact(jValue)
      case None => compact(JNothing)
    }
  }

  post("/api/v1/player/matchup") {

    val player1Name = params("player1")
    val player2Name = params("player2")

    val playerWonJValue = for {
      player1Id <- StatsNBA.playerId(player1Name)
      player2Id <- StatsNBA.playerId(player2Name)
    } yield PlayerMatchup.result((Player(player1Name, player1Id), Player(player2Name, player2Id)))

    playerWonJValue match {
      case Some(jValue) => compact(jValue)
      case None => compact(JNothing)
    }
  }

//////////////////////// Tournament ////////////////////////////////

  post("/api/v1/team/tournament") {
    val teams = for {
      team <- multiParams("team").toList
      teamId <- StatsNBA.teamId(team)
    } yield Team(team, teamId)

    Tournament.winner(teams) match {
      case Some(team) => compact(("result" -> team.name) ~ ("time" -> Instant.now.toString))
      case None => compact(JNothing)
    }
  }

}

///////////////////////////////////////////////////////////////////////

case class Team(val name: String, val teamId: String)

case class Player(val name: String, val playerId: String)

//////////////////////////////////////////////////////////////////////

object TeamMatchup {
  def result(pair: Pair[Team, Team]): JValue = {
    /*
     * TODO:
     * 1) Query stats.nba.com in order to get information about two teams
     * 2) Perform analytics to determine the result
     */
    val team1Js = StatsNBA.team(pair._1)
    val team2Js = StatsNBA.team(pair._2)

    val prior = Instant.now

    // TODO: add algorithm for calculating the winner
    var winner: Option[Team] = Some(pair._1)

    winner match {
      case Some(winner) => ("response" -> winner.name) ~ ("latency" -> ChronoUnit.MILLIS.between(prior, Instant.now))
      case None => JNothing
    }
  }
}

object Tournament {
  def winner(teams: List[Team]): Option[Team] = {
    /*
     *  TODO:
     *  1) Query stats.nba.com in order to get information about all teams
     *  2) Perform anaylitcs to determine the winner of the tournament
     */
    teams.sortBy(_.name) match {
      case firstTeam :: rest => {

        val winner = firstTeam
        Some(winner)
      }
      case _ => None
    }
  }
}

object PlayerMatchup {

  case class PlayerStats(val points: Option[Double], val assists: Option[Double], val rebounds: Option[Double])

  // TODO: add more parameters to player's mojo calculation
  def playerMojo(playerStats: PlayerStats): Option[Double] = {
    for {
      playerPoints <- playerStats.points
      playerRebounds <- playerStats.rebounds
      playerAssists <- playerStats.assists
    } yield (playerPoints + playerAssists + playerRebounds)
  }

  def result(pair: Pair[Player, Player]): JValue = {
    /*
     * 1) Query stats.nba.com in order to get information about two players
     * 2) Perform analytics to determine the result
     */
    val player1Js = StatsNBA.player(pair._1)
    val player2Js = StatsNBA.player(pair._2)

    val prior = Instant.now

    val player1Stats = playerStats(player1Js)
    val player2Stats = playerStats(player2Js)

    val winnerOpt = for {
      player1Mojo <- playerMojo(player1Stats)
      player2Mojo <- playerMojo(player2Stats)
    } yield if (player1Mojo > player2Mojo) pair._1 else pair._2

    winnerOpt match {
      case Some(winner) => ("response" -> winner.name) ~ ("latency" -> ChronoUnit.MILLIS.between(prior, Instant.now))
      case None => JNothing
    }
  }

  implicit val formats = DefaultFormats

  def playerStats(playerJs: JValue): PlayerStats = {
    val resultSetsJs = playerJs \ "response" \ "resultSets"

    val rowSetJs = resultSetsJs(1) \ "rowSet"

    val statsJs = rowSetJs(0)

    val points = statsJs(3).extractOpt[Double]

    val assists = statsJs(4).extractOpt[Double]

    val rebounds = statsJs(5).extractOpt[Double]

    PlayerStats(points, assists, rebounds)
  }
}

object StatsNBA {

  val statsNbaURL = "http://stats.nba.com/stats/"

  val teamRosterURI = "commonteamroster/?"

  val currentSeasonURI = "&Season=2015-16"

  val teamNameToId = Map(
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

  def teamId(teamName: String): Option[String] = teamNameToId.get(teamName)

  def teamIDURI(teamId: String) = s"TeamID=$teamId"

  def team(teamId: String): JValue = {

    val teamStatsNBAURL = statsNbaURL + teamRosterURI + teamIDURI(teamId) + currentSeasonURI

    val prior = Instant.now

    val response = parse(Source.fromURL(teamStatsNBAURL, StandardCharsets.UTF_8.name()).mkString)

    ("response" -> response) ~ ("latency" -> ChronoUnit.MILLIS.between(prior, Instant.now))
  }

  def team(t: Team): JValue = team(t.teamId)

  def teamByName(teamName: String): JValue =
    teamId(teamName).map(team(_))

  ///////////////////////

  // TODO: list all players
  val playerNameToId = Map(
    "LeBronJames" -> "2544",
    "DarkoMilicic" -> "2545",
    "CarmeloAnthony" -> "2546",
    "ChrisBosh" -> "2547",
    "DwyaneWade" -> "2548"
  )

  def playerId(playerName: String): Option[String] = playerNameToId.get(playerName)

  def playerByName(playerName: String): JValue =
    playerId(playerName).map(player(_))

  def playerURI = "commonplayerinfo/?"

  def playerIDURI(playerId: String) = s"PlayerId=$playerId"

  def player(playerId: String): JValue = {
    val playerStatsNBAURL = statsNbaURL + playerURI + playerIDURI(playerId)

    val prior = Instant.now

    val response = parse(Source.fromURL(playerStatsNBAURL, StandardCharsets.UTF_8.name()).mkString)

    ("response" -> response) ~ ("latency" -> ChronoUnit.MILLIS.between(prior, Instant.now))
  }

  def player(p: Player): JValue = player(p.playerId)

  ///////////////////////

  def regularSeasonTypeURI = "&SeasonType=Regular+Season"

  def nbaLeagueURI = "&LeagueId=00"
}
