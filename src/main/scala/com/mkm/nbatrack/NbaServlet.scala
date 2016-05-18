package com.mkm.nbatrack

import org.scalatra._

class NbaServlet extends NbatrackStack {

  get("/") {
    <html>
      <body>
        <h1>Hello, world!</h1>
        Say <a href="hello-scalate">hello to Scalate</a>.

        Main page will contain two submit forms for two teams.
      </body>
    </html>
  }

  before("/api/v1/matchup") {
    contentType = "application/json"
  }

  post("/api/v1/matchup") {
    // TODO: add pattern-match check agains a list of allowed teams
    val team1 = params("team1")
    val team2 = params("team2")

    println(s"team1 = $team1 and team2 = $team2")

    // TODO: redirect to /result with template accepting Matchup.result object
    Matchup(team1, team2).result()
  }


  post("/api/v1/tournament") {
    for (team <- multiParams("team")) {
      // TODO: add pattern-match check agains a list of allowed teams
      Tournament.add(team) 
    }
    println(s"Touranment: ${Tournament.teams}")
  }

}

object Matchup(val team1:String, val team2: String) {
  def result() {
    // Query nba.com with this two teams

  }
}

object Tournament {
  var teams = List[String]()

  def add(team: String): Unit = { teams ::= team }
}
