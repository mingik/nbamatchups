package com.mkm.nbatrack

import org.scalatra.test.specs2._

// For more on Specs2, see http://etorreborre.github.com/specs2/guide/org.specs2.guide.QuickStart.html
class NbaServletSpec extends ScalatraSpec { def is =
  "GET /api/v1/* on NbaServle"                     ^
  "/ should return status 200"                     ! root200^
  "/team/:team should return valid JSON"           ! getTeamValid^
  "/team/:invalidteam should return empty JSON"    ! getTeamInvalid^
  "/player/:player should return valid JSON"       ! getPlayerValid^
  "/player/:invlaidplaye should return empty JSON" ! getPlayerInvalid^
  "/team/matchup should return valid JSON"         ! postTeamMatchupValid^
  "/team/matchup invalid should return valid JSON" ! postTeamMatchupInvalid^
  "/player/matchup should return valid JSON"       ! postPlayerMatchupValid^
  "/player/matchup invalid should return valid JSON" ! postPlayerMatchupInvalid^
  "/team/tournament should return valid JSON"      ! postTournamentValid^
  "/team/tournament invalid should return valid JSON" ! postTournamentInvalid^
                                                   end

  addServlet(classOf[NbaServlet], "/*")

  def root200 = get("/") {
    status must_== 200
  }

  def getTeamValid = get("/api/v1/team/BostonCeltics") {
    status must_== 200
    body must contain("result")
    body must contain("resource")
    body must contain("parameters")
    body must contain("1610612738")
    body must contain(StatsNBA.teamNameToId("BostonCeltics"))
    body must contain("time")
  }

  def getTeamInvalid = get("/api/v1/team/BC") {
    status must_== 200
    body.size must_== 0
  }

  def getPlayerValid = get("/api/v1/player/LeBronJames") {
    status must_== 200
    body must contain("result")
    body must contain("resource")
    body must contain("parameters")
    body must contain(StatsNBA.playerNameToId("LeBronJames"))
    body must contain("time")
  }

  def getPlayerInvalid = get("/api/v1/player/LBJ") {
    status must_== 200
    body.size must_== 0
  }

  def postTeamMatchupValid = post("/api/v1/team/matchup", Map("team1" -> "BostonCeltics", "team2" -> "LosAngelesLakers")) {
    status must_== 200
    body must contain("BostonCeltics")
  }

  def postTeamMatchupInvalid = post("/api/v1/team/matchup", Map("team1" -> "BC", "team2" -> "LosAngelesLakers")) {
    status must_== 200
    body.size must_== 0
  }

  def postPlayerMatchupValid = post("/api/v1/player/matchup", Map("player1" -> "LeBronJames", "player2" -> "LeBronJames")) {
    status must_== 200
    body must contain("LeBronJames")
  }

  def postPlayerMatchupInvalid = post("/api/v1/player/matchup", Map("player1" -> "LBJ", "player2" -> "LBJ")) {
    status must_== 200
    body.size must_== 0
  }

   def postTournamentValid = post("/api/v1/team/tournament", ("team","BostonCeltics"), ("a","b"), ("team","LosAngelesLakers"), ("c","d"), ("team","NewYorkKnicks")) {
     status must_== 200
     body must contain("BostonCeltics")
   }

   def postTournamentInvalid = post("/api/v1/team/tournament", ("team","BC"), ("a","b"), ("team","LAL"), ("c","d"), ("team","NYK")) {
     status must_== 200
     println(s"body = $body")
     body.size must_== 0
   }
}
