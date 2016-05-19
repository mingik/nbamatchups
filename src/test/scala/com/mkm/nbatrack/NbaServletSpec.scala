package com.mkm.nbatrack

import org.scalatra.test.specs2._

// For more on Specs2, see http://etorreborre.github.com/specs2/guide/org.specs2.guide.QuickStart.html
class NbaServletSpec extends ScalatraSpec { def is =
  "GET /api/v1/* on NbaServle"              ^
  "/ should return status 200"              ! root200^
  "/team/:team should return valid JSON"    ! getTeam^
  "/matchup should return valid JSON"       ! postMatchup^
  "/tournament should return valid JSON"    ! postTournament^
                                              end

  addServlet(classOf[NbaServlet], "/*")

  def root200 = get("/") {
    status must_== 200
  }

  def getTeam = get("/api/v1/team/BostonCeltics") {
    status must_== 200
    body must contain("teamId")
  }

  def postMatchup = post("/api/v1/matchup", Map("team1" -> "BostonCeltics", "team2" -> "LosAngelesLakers")) {
    status must_== 200
  }

   def postTournament = post("/api/v1/tournament", Map("team" -> "BostonCeltics"), Map("team" -> "LosAngelesLakers")) {
     status must_== 200
   }
}
