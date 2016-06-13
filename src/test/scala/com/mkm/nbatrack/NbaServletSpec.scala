package com.mkm.statsdontlie.nba

import org.scalatra.test.specs2._

import org.json4s._
import org.json4s.jackson.JsonMethods
import org.json4s.jackson.JsonMethods._

trait JsonBodySupport { self: ScalatraSpec =>

  def jsonBody: JValue = if (body.size >0) JsonMethods.parse(body) else JNothing

}

// For more on Specs2, see http://etorreborre.github.com/specs2/guide/org.specs2.guide.QuickStart.html
class NbaServletSpec extends ScalatraSpec with JsonBodySupport { def is =
  "GET /api/v1/nba/* on NbaServle"                     ^
  "/nba should return status 200"                     ! root200^
  "/nba/team/:team should return valid JSON"           ! getTeamValid^
  "/nba/team/:invalidteam should return empty JSON"    ! getTeamInvalid^
  "/nba/player/:player should return valid JSON"       ! getPlayerValid^
  "/nba/player/:invlaidplaye should return empty JSON" ! getPlayerInvalid^
  "/nba/team/matchup should return valid JSON"         ! postTeamMatchupValid^
  "/nba/team/matchup invalid should return valid JSON" ! postTeamMatchupInvalid^
  "/nba/player/matchup should return valid JSON"       ! postPlayerMatchupValid1^
  "/nba/player/matchup should return valid JSON"       ! postPlayerMatchupValid2^
  "/nba/player/matchup invalid should return valid JSON" ! postPlayerMatchupInvalid^
  "/nba/team/tournament should return valid JSON"      ! postTournamentValid^
  "/nba/team/tournament invalid should return valid JSON" ! postTournamentInvalid^
                                                   end

  addServlet(classOf[NbaServlet], "/*")

  def root200 = get("/nba") {
    status must_== 200
  }

  implicit val formats = DefaultFormats

  def getTeamValid = get("/api/v1/nba/team/BostonCeltics") {
    status must_== 200

    val latencyJValue = jsonBody \ "latency"
    latencyJValue must haveClass[JDouble]
    latencyJValue.extract[Double] must beGreaterThan(0.0) 

    val responseJValue = jsonBody \ "response" 
    responseJValue must haveClass[JObject]
    responseJValue \ "resource" must haveClass[JString]
    responseJValue \ "parameters" must haveClass[JObject]

    val teamIDJValue = responseJValue \ "parameters" \ "TeamID"
    teamIDJValue.extract[Int] must_== 1610612738
    teamIDJValue.extract[Int] must_== StatsNBA.teamNameToId("BostonCeltics").toInt

    val resultSetsJValue = responseJValue \ "resultSets"
    resultSetsJValue must haveClass[JArray]
    resultSetsJValue.asInstanceOf[JArray].values.size must beGreaterThan(0)
  }

  def getTeamInvalid = get("/api/v1/nba/team/BC") {
    status must_== 200
    
    jsonBody must_== JNothing
  }

  def getPlayerValid = get("/api/v1/nba/player/LeBronJames") {
    status must_== 200

    val latencyJValue = jsonBody \ "latency"
    latencyJValue must haveClass[JDouble]
    latencyJValue.extract[Double] must beGreaterThan(0.0) 

    val responseJValue = jsonBody \ "response"
    responseJValue must haveClass[JObject]
    responseJValue \ "resource" must haveClass[JString]
    responseJValue \ "parameters" must haveClass[JObject]

    val playerIDValue = responseJValue \ "parameters" \ "PlayerID"
    playerIDValue.extract[Int] must_== 2544
    playerIDValue.extract[Int] must_== StatsNBA.playerNameToId("LeBronJames")

    val resultSetsJValue = responseJValue \ "resultSets"
    resultSetsJValue must haveClass[JArray]
    resultSetsJValue.asInstanceOf[JArray].values.size must beGreaterThan(0)
  }

  def getPlayerInvalid = get("/api/v1/nba/player/LBJ") {
    status must_== 200
    
    jsonBody must_== JNothing
  }

  def postTeamMatchupValid = post("/api/v1/nba/team/matchup", Map("team1" -> "BostonCeltics", "team2" -> "LosAngelesLakers")) {
    status must_== 200
    
    body must contain("BostonCeltics")
  }

  def postTeamMatchupInvalid = post("/api/v1/nba/team/matchup", Map("team1" -> "BC", "team2" -> "LosAngelesLakers")) {
    status must_== 200
    
    jsonBody must_== JNothing
  }

  def postPlayerMatchupValid1 = post("/api/v1/nba/player/matchup", Map("player1" -> "AndreIguodala", "player2" -> "CarmeloAnthony")) {
    status must_== 200
    
    body must contain("CarmeloAnthony")
  }

  def postPlayerMatchupValid2 = post("/api/v1/nba/player/matchup", Map("player1" -> "LeBronJames", "player2" -> "AndreIguodala")) {
    status must_== 200

    body must contain("LeBronJames")
  }

  def postPlayerMatchupInvalid = post("/api/v1/nba/player/matchup", Map("player1" -> "LBJ", "player2" -> "LBJ")) {
    status must_== 200
    
    body.size must_== 0
  }

   def postTournamentValid = post("/api/v1/nba/team/tournament", ("team","BostonCeltics"), ("a","b"), ("team","LosAngelesLakers"), ("c","d"), ("team","NewYorkKnicks")) {
     status must_== 200
     
     body must contain("BostonCeltics")
   }

   def postTournamentInvalid = post("/api/v1/nba/team/tournament", ("team","BC"), ("a","b"), ("team","LAL"), ("c","d"), ("team","NYK")) {
     status must_== 200
     
     body.size must_== 0
   }
}
