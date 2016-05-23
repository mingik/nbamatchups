package com.mkm.nbatrack

import org.scalatra.test.specs2._

// For more on Specs2, see http://etorreborre.github.com/specs2/guide/org.specs2.guide.QuickStart.html
class NbaServletSpec extends ScalatraSpec { def is =
  "GET /api/v1/* on NbaServle"                     ^
  "/ should return status 200"                     ! root200^
  "/team/:team should return valid JSON"           ! getTeamValid^
  "/team/:invalidteam should return empty JSON"    ! getTeamInvalid^
  "/matchup should return valid JSON"              ! postMatchupValid^
  "/matchup invalid should return valid JSON"      ! postMatchupInvalid^
  "/tournament should return valid JSON"           ! postTournamentValid^
  "/tournament invalid should return valid JSON"   ! postTournamentInvalid^
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
    body must contain(Teams.nameToId("BostonCeltics"))
    body must contain("time")
  }

  def getTeamInvalid = get("/api/v1/team/BC") {
    status must_== 200
    body.size must_== 0
  }

  def postMatchupValid = post("/api/v1/matchup", Map("team1" -> "BostonCeltics", "team2" -> "LosAngelesLakers")) {
    status must_== 200
    body must contain("BostonCeltics")
  }

  def postMatchupInvalid = post("/api/v1/matchup", Map("team1" -> "BC", "team2" -> "LosAngelesLakers")) {
    status must_== 200
    body.size must_== 0
  }

   def postTournamentValid = post("/api/v1/tournament", ("team","BostonCeltics"), ("a","b"), ("team","LosAngelesLakers"), ("c","d"), ("team","NewYorkKnicks")) {
     status must_== 200
     body must contain("BostonCeltics")
   }

   def postTournamentInvalid = post("/api/v1/tournament", ("team","BC"), ("a","b"), ("team","LAL"), ("c","d"), ("team","NYK")) {
     status must_== 200
     println(s"body = $body")
     body.size must_== 0
   }
}
