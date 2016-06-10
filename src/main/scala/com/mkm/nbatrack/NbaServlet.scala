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

/////////////////////////// external API //////////////////////////////////

  get("/") {
    <html>
      <body>
        <h1>Try <a href="/team/matchup/random">random</a> team matchup!</h1>
        <h1>Try <a href="/player/matchup/random">random</a> player matchup!</h1>
	
	<h2><a href="/team/matchup">Create your own team matchup. </a></h2>

	<br/>

	<h2><a href="/player/matchup">Create your own player matchup. </a></h2>
      </body>
    </html>
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

  get("/team/matchup") {
    contentType="text/html"

    // TODO: pass List[Team] to template instead
    layoutTemplate("team_matchup.html", "teams" -> StatsNBA.teamNameToId.keys.toList)
  }

  get("/team/matchup/random") {
    contentType="text/html"

    val randomIdxOne = scala.util.Random.nextInt(StatsNBA.teams.size)
    val randomIdxTwo = if (randomIdxOne == 0) (randomIdxOne + 1) else (randomIdxOne - 1)

    val matchup = List(
      StatsNBA.teams(randomIdxOne),
      StatsNBA.teams(randomIdxTwo))

    layoutTemplate("random.html", "matchup" -> matchup)
  }

  post("/api/v1/team/matchup") {

    val team1Name = params("team1")
    val team2Name = params("team2")

    val teamWonJValue = for {
      team1Id <- StatsNBA.teamId(team1Name)
      team2Id <- StatsNBA.teamId(team2Name)
    } yield TeamMatchup.resultJV((Team(team1Name, team2Id), Team(team2Name, team2Id)))

    teamWonJValue match {
      case Some(jValue) => compact(jValue)
      case None => compact(JNothing)
    }
  }

 ////////////////////////////

  get("/player/matchup") {
    contentType="text/html"

    // TODO: pass List[Player] to template instead
    layoutTemplate("player_matchup.html", "players" -> StatsNBA.playerNameToId.keys.toList)
  }

  get("/player/matchup/random") {
    contentType="text/html"

    val randomIdxOne = scala.util.Random.nextInt(StatsNBA.players.size)
    val randomIdxTwo = if (randomIdxOne == 0) (randomIdxOne + 1) else (randomIdxOne - 1)

    val matchup = List(
      StatsNBA.players(randomIdxOne),
      StatsNBA.players(randomIdxTwo))

    layoutTemplate("random.html", "matchup" -> matchup)
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
  def result(pair: Pair[Team, Team]): Option[Team] = {
    /*
     * TODO:
     * 1) Query stats.nba.com in order to get information about two teams
     * 2) Perform analytics to determine the result
     */
    val team1Js = StatsNBA.team(pair._1)
    val team2Js = StatsNBA.team(pair._2)

    val prior = Instant.now

    // TODO: add algorithm for calculating the winner
    Some(pair._1)
  }

  def resultJV(pair: Pair[Team, Team]): JValue = {
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

////////////////////////////////////////////////////////////////////

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

  val teams = teamNameToId.keys.toArray

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

  ///////////////////////////////////////////////////////////////

  // TODO: list all players
  val playerNameToId = Map(
    "QuincyAcy" -> "203112",
    "JordanAdams" -> "203919",
    "StevenAdams" -> "203500",
    "ArronAfflalo" -> "201167",
    "AlexisAjinca" -> "201582",
    "ColeAldrich" -> "202332",
    "LaMarcusAldridge" -> "200746",
    "CliffAlexander" -> "1626146",
    "LavoyAllen" -> "202730",
    "TonyAllen" -> "2754",
    "Al-FarouqAminu" -> "202329",
    "LouAmundson" -> "200811",
    "ChrisAndersen" -> "2365",
    "AlanAnderson" -> "101187",
    "JamesAnderson" -> "202341",
    "JustinAnderson" -> "1626147",
    "KyleAnderson" -> "203937",
    "RyanAnderson" -> "201583",
    "GiannisAntetokounmpo" -> "203507",
    "ThanasisAntetokounmpo" -> "203648",
    "CarmeloAnthony" -> "2546",
    "JoelAnthony" -> "201202",
    "KeithAppling" -> "203951",
    "TrevorAriza" -> "2772",
    "DarrellArthur" -> "201589",
    "OmerAsik" -> "201600",
    "D.J.Augustin" -> "201571",
    "JeffAyres" -> "201965",
    "LukeBabbitt" -> "202337",
    "CameronBairstow" -> "203946",
    "LeandroBarbosa" -> "2571",
    "JoseJuanBarea" -> "200826",
    "AndreaBargnani" -> "200745",
    "HarrisonBarnes" -> "203084",
    "MattBarnes" -> "2440",
    "WillBarton" -> "203115",
    "BrandonBass" -> "101138",
    "NicolasBatum" -> "201587",
    "JerrydBayless" -> "201573",
    "AronBaynes" -> "203382",
    "KentBazemore" -> "203145",
    "BradleyBeal" -> "203078",
    "MichaelBeasley" -> "201563",
    "MarcoBelinelli" -> "201158",
    "AnthonyBennett" -> "203461",
    "PatrickBeverley" -> "201976",
    "BismackBiyombo" -> "202687",
    "NemanjaBjelica" -> "202357",
    "TarikBlack" -> "204028",
    "DeJuanBlair" -> "201971",
    "SteveBlake" -> "2581",
    "EricBledsoe" -> "202339",
    "BojanBogdanovic" -> "202711",
    "AndrewBogut" -> "101106",
    "MattBonner" -> "2588",
    "DevinBooker" -> "1626164",
    "TrevorBooker" -> "202344",
    "ChrisBosh" -> "2547",
    "AveryBradley" -> "202340",
    "EltonBrand" -> "1882",
    "CoreyBrewer" -> "201147",
    "AaronBrooks" -> "201166",
    "AnthonyBrown" -> "1626148",
    "LorenzoBrown" -> "203485",
    "MarkelBrown" -> "203900",
    "KobeBryant" -> "977",
    "ChaseBudinger" -> "201978",
    "ReggieBullock" -> "203493",
    "TreyBurke" -> "203504",
    "AlecBurks" -> "202692",
    "CaronButler" -> "2406",
    "JimmyButler" -> "202710",
    "RasualButler" -> "2446",
    "BrunoCaboclo" -> "203998",
    "JoseCalderon" -> "101181",
    "KentaviousCaldwell-Pope" -> "203484",
    "IsaiahCanaan" -> "203477",
    "ClintCapela" -> "203991",
    "DeMarreCarroll" -> "201960",
    "VinceCarter" -> "1713",
    "MichaelCarter-Williams" -> "203487",
    "OmriCasspi" -> "201956",
    "WillieCauley-Stein" -> "1626161",
    "MarioChalmers" -> "201596",
    "TysonChandler" -> "2199",
    "WilsonChandler" -> "201163",
    "RakeemChristmas" -> "1626176",
    "IanClark" -> "203546",
    "CotyClarke" -> "1626262",
    "JordanClarkson" -> "203903",
    "NorrisCole" -> "202708",
    "DarrenCollison" -> "201954",
    "NickCollison" -> "2555",
    "MikeConley" -> "201144",
    "PatConnaughton" -> "1626192",
    "ChrisCopeland" -> "203142",
    "BryceCotton" -> "203955",
    "DeMarcusCousins" -> "202326",
    "RobertCovington" -> "203496",
    "AllenCrabbe" -> "203459",
    "JamalCrawford" -> "2037",
    "JaeCrowder" -> "203109",
    "DanteCunningham" -> "201967",
    "JaredCunningham" -> "203099",
    "SethCurry" -> "203552",
    "StephenCurry" -> "201939",
    "TroyDaniels" -> "203584",
    "AnthonyDavis" -> "203076",
    "EdDavis" -> "202334",
    "BrandenDawson" -> "1626183",
    "DeMarDeRozan" -> "201942",
    "DewayneDedmon" -> "203473",
    "BryceDejean-Jones" -> "1626214",
    "SamDekker" -> "1626155",
    "MatthewDellavedova" -> "203521",
    "LuolDeng" -> "2736",
    "BorisDiaw" -> "2564",
    "GorguiDieng" -> "203476",
    "SpencerDinwiddie" -> "203915",
    "ToneyDouglas" -> "201962",
    "GoranDragic" -> "201609",
    "AndreDrummond" -> "203083",
    "JaredDudley" -> "201162",
    "DujeDukan" -> "1626251",
    "TimDuncan" -> "1495",
    "MikeDunleavy" -> "2399",
    "KevinDurant" -> "201142",
    "CleanthonyEarly" -> "203921",
    "JarellEddie" -> "204067",
    "WayneEllington" -> "201961",
    "MontaEllis" -> "101145",
    "JoelEmbiid" -> "203954",
    "JamesEnnis" -> "203516",
    "TylerEnnis" -> "203898",
    "JeremyEvans" -> "202379",
    "TyrekeEvans" -> "201936",
    "DanteExum" -> "203957",
    "FestusEzeli" -> "203105",
    "KennethFaried" -> "202702",
    "JordanFarmar" -> "200770",
    "DerrickFavors" -> "202324",
    "CristianoFelicio" -> "1626245",
    "RaymondFelton" -> "101109",
    "EvanFournier" -> "203095",
    "RandyFoye" -> "200751",
    "TimFrazier" -> "204025",
    "JimmerFredette" -> "202690",
    "ChanningFrye" -> "101112",
    "DaniloGallinari" -> "201568",
    "LangstonGalloway" -> "204038",
    "KevinGarnett" -> "708",
    "MarcGasol" -> "201188",
    "PauGasol" -> "2200",
    "RudyGay" -> "200752",
    "AlonzoGee" -> "202087",
    "PaulGeorge" -> "202331",
    "TajGibson" -> "201959",
    "ManuGinobili" -> "1938",
    "RudyGobert" -> "203497",
    "DrewGooden" -> "2400",
    "ArchieGoodwin" -> "203462",
    "AaronGordon" -> "203932",
    "EricGordon" -> "201569",
    "MarcinGortat" -> "101162",
    "AndrewGoudelock" -> "202726",
    "JeramiGrant" -> "203924",
    "JerianGrant" -> "1626170",
    "DannyGreen" -> "201980",
    "DraymondGreen" -> "203110",
    "ErickGreen" -> "203475",
    "GeraldGreen" -> "101123",
    "JaMychalGreen" -> "203210",
    "JeffGreen" -> "201145",
    "BlakeGriffin" -> "201933",
    "JorgeGutierrez" -> "203268",
    "PJHairston" -> "203798",
    "JordanHamilton" -> "202706",
    "TylerHansbrough" -> "201946",
    "TimHardawayJr." -> "203501",
    "JamesHarden" -> "201935",
    "MauriceHarkless" -> "203090",
    "JustinHarper" -> "202712",
    "MontrezlHarrell" -> "1626149",
    "DevinHarris" -> "2734",
    "GaryHarris" -> "203914",
    "JoeHarris" -> "203925",
    "TobiasHarris" -> "202699",
    "AaronHarrison" -> "1626151",
    "UdonisHaslem" -> "2617",
    "SpencerHawes" -> "201150",
    "CharlesHayes" -> "101236",
    "GordonHayward" -> "202330",
    "GeraldHenderson" -> "201945",
    "JohnHenson" -> "203089",
    "MarioHezonja" -> "1626209",
    "RoyHibbert" -> "201579",
    "JJHickson" -> "201581",
    "Nene" -> "2403",
    "GeorgeHill" -> "201588",
    "JordanHill" -> "201941",
    "SolomonHill" -> "203524",
    "DarrunHilliard" -> "1626199",
    "KirkHinrich" -> "2550",
    "JrueHoliday" -> "201950",
    "JustinHoliday" -> "203200",
    "JohnHolland" -> "204066",
    "RyanHollins" -> "200797",
    "RondaeHollis-Jefferson" -> "1626178",
    "RichaunHolmes" -> "1626158",
    "RodneyHood" -> "203918",
    "AlHorford" -> "201143",
    "DwightHoward" -> "2730",
    "MarceloHuertas" -> "1626273",
    "JoshHuestis" -> "203962",
    "KrisHumphries" -> "2743",
    "RJHunter" -> "1626154",
    "SergeIbaka" -> "201586",
    "AndreIguodala" -> "2738",
    "ErsanIlyasova" -> "101141",
    "JoeIngles" -> "204060",
    "DamienInglis" -> "203996",
    "KyrieIrving" -> "202681",
    "JarrettJack" -> "101127",
    "ReggieJackson" -> "202704",
    "LeBronJames" -> "2544",
    "AlJefferson" -> "2744",
    "CoryJefferson" -> "203928",
    "RichardJefferson" -> "2210",
    "JohnJenkins" -> "203098",
    "BrandonJennings" -> "201943",
    "JonasJerebko" -> "201973",
    "AmirJohnson" -> "101161",
    "ChrisJohnson" -> "203187",
    "JamesJohnson" -> "201949",
    "JoeJohnson" -> "2207",
    "OrlandoJohnson" -> "203111",
    "StanleyJohnson" -> "1626169",
    "TylerJohnson" -> "204020",
    "WesleyJohnson" -> "202325",
    "NikolaJokic" -> "203999",
    "DahntayJones" -> "2563",
    "JamesJones" -> "2592",
    "TerrenceJones" -> "203093",
    "TyusJones" -> "1626145",
    "DeAndreJordan" -> "201599",
    "CoryJoseph" -> "202709",
    "ChrisKaman" -> "2549",
    "FrankKaminsky" -> "1626163",
    "EnesKanter" -> "202683",
    "SergeyKarasev" -> "203508",
    "SashaKaun" -> "201619",
    "RyanKelly" -> "203527",
    "MichaelKidd-Gilchrist" -> "203077",
    "SeanKilpatrick" -> "203930",
    "BrandonKnight" -> "202688",
    "KyleKorver" -> "2594",
    "KostaKoufos" -> "201585",
    "ZachLaVine" -> "203897",
    "JeremyLamb" -> "203087",
    "CarlLandry" -> "201171",
    "ShaneLarkin" -> "203499",
    "JoffreyLauvergne" -> "203530",
    "TyLawson" -> "201951",
    "CourtneyLee" -> "201584",
    "DavidLee" -> "101135",
    "AlexLen" -> "203458",
    "KawhiLeonard" -> "202695",
    "MeyersLeonard" -> "203086",
    "JonLeuer" -> "202720",
    "DamianLillard" -> "203081",
    "JeremyLin" -> "202391",
    "ShaunLivingston" -> "2733",
    "KevonLooney" -> "1626172",
    "BrookLopez" -> "201572",
    "RobinLopez" -> "201577",
    "KevinLove" -> "201567",
    "KyleLowry" -> "200768",
    "TreyLyles" -> "1626168",
    "ShelvinMack" -> "202714",
    "IanMahinmi" -> "101133",
    "DevynMarble" -> "203906",
    "BobanMarjanovic" -> "1626246",
    "KendallMarshall" -> "203088",
    "JarellMartin" -> "1626185",
    "KevinMartin" -> "2755",
    "WesleyMatthews" -> "202083",
    "O.J.Mayo" -> "201564",
    "LucMbahaMoute" -> "201601",
    "JamesMichaelMcAdoo" -> "203949",
    "RayMcCallum" -> "203492",
    "CJMcCollum" -> "203468",
    "TJMcConnell" -> "204456",
    "ChrisMcCullough" -> "1626191",
    "KJMcDaniels" -> "203909",
    "DougMcDermott" -> "203926",
    "MitchMcGary" -> "203956",
    "JaValeMcGee" -> "201580",
    "BenMcLemore" -> "203463",
    "JordanMcRae" -> "203895",
    "JoshMcRoberts" -> "201177",
    "JodieMeeks" -> "201975",
    "SalahMejri" -> "1626257",
    "JordanMickey" -> "1626175",
    "KhrisMiddleton" -> "203114",
    "CJMiles" -> "101139",
    "AndreMiller" -> "1889",
    "MikeMiller" -> "2034",
    "PattyMills" -> "201988",
    "ElijahMillsap" -> "202407",
    "PaulMillsap" -> "200794",
    "NikolaMirotic" -> "202703",
    "NazrMohammed" -> "1737",
    "GregMonroe" -> "202328",
    "LuisMontero" -> "1626242",
    "E'TwaunMoore" -> "202734",
    "EricMoreland" -> "203961",
    "MarcusMorris" -> "202694",
    "MarkieffMorris" -> "202693",
    "AnthonyMorrow" -> "201627",
    "DonatasMotiejunas" -> "202700",
    "TimofeyMozgov" -> "202389",
    "EmmanuelMudiay" -> "1626144",
    "ShabazzMuhammad" -> "203498",
    "XavierMunford" -> "204098",
    "MikeMuscala" -> "203488",
    "LarryNanceJr." -> "1626204",
    "ShabazzNapier" -> "203894",
    "GaryNeal" -> "202390",
    "JameerNelson" -> "2749",
    "RaulNeto" -> "203526",
    "AndrewNicholson" -> "203094",
    "JoakimNoah" -> "201149",
    "NerlensNoel" -> "203457",
    "LucasNogueira" -> "203512",
    "SteveNovak" -> "200779",
    "DirkNowitzki" -> "1717",
    "JusufNurkic" -> "203994",
    "J.J.O'Brien" -> "1626266",
    "JohnnyO'Bryant" -> "203948",
    "KyleO'Quinn" -> "203124",
    "JahlilOkafor" -> "1626143",
    "VictorOladipo" -> "203506",
    "KellyOlynyk" -> "203482",
    "KellyOubre" -> "1626162",
    "ZazaPachulia" -> "2585",
    "KostasPapanikolaou" -> "203123",
    "JabariParker" -> "203953",
    "TonyParker" -> "2225",
    "ChandlerParsons" -> "202718",
    "LamarPatterson" -> "203934",
    "PatrickPatterson" -> "202335",
    "ChrisPaul" -> "101108",
    "AdreianPayne" -> "203940",
    "CameronPayne" -> "1626166",
    "ElfridPayton" -> "203901",
    "NikolaPekovic" -> "201593",
    "KendrickPerkins" -> "2570",
    "PaulPierce" -> "1718",
    "TiborPleiss" -> "202353",
    "MasonPlumlee" -> "203486",
    "MilesPlumlee" -> "203101",
    "QuincyPondexter" -> "202347",
    "OttoPorter" -> "203490",
    "BobbyPortis" -> "1626171",
    "KristapsPorzingis" -> "204001",
    "DwightPowell" -> "203939",
    "NormanPowell" -> "1626181",
    "PhilPressey" -> "203515",
    "RonniePrice" -> "101179",
    "PabloPrigioni" -> "203143",
    "TayshaunPrince" -> "2419",
    "JuliusRandle" -> "203944",
    "ZachRandolph" -> "2216",
    "JJRedick" -> "200755",
    "WillieReed" -> "203186",
    "JoshRichardson" -> "1626196",
    "AustinRivers" -> "203085",
    "AndreRoberson" -> "203460",
    "BrianRoberts" -> "203148",
    "GlennRobinson" -> "203922",
    "NateRobinson" -> "101126",
    "ThomasRobinson" -> "203080",
    "RajonRondo" -> "200765",
    "DerrickRose" -> "201565",
    "TerrenceRoss" -> "203082",
    "TerryRozier" -> "1626179",
    "RickyRubio" -> "201937",
    "DamjanRudez" -> "204014",
    "BrandonRush" -> "201575",
    "D'AngeloRussell" -> "1626156",
    "RobertSacre" -> "203135",
    "JaKarrSampson" -> "203960",
    "DennisSchroder" -> "203471",
    "LuisScola" -> "2449",
    "MikeScott" -> "203118",
    "ThaboSefolosha" -> "200757",
    "KevinSeraphin" -> "202338",
    "RamonSessions" -> "201196",
    "ImanShumpert" -> "202697",
    "JonathonSimmons" -> "203613",
    "HenrySims" -> "203156",
    "KyleSingler" -> "202713",
    "DonaldSloan" -> "202388",
    "MarcusSmart" -> "203935",
    "GregSmith" -> "202962",
    "IshSmith" -> "202397",
    "J.R.Smith" -> "2747",
    "JasonSmith" -> "201160",
    "JoshSmith" -> "2746",
    "RussSmith" -> "203893",
    "TonySnell" -> "203503",
    "MarreeseSpeights" -> "201578",
    "TiagoSplitter" -> "201168",
    "NikStauskas" -> "203917",
    "LanceStephenson" -> "202362",
    "AlexStepheson" -> "1627293",
    "JarnellStokes" -> "203950",
    "Amar'eStoudemire" -> "2405",
    "RodneyStuckey" -> "201155",
    "JaredSullinger" -> "203096",
    "WalterTavares" -> "204002",
    "JeffTeague" -> "201952",
    "MirzaTeletovic" -> "203141",
    "GarrettTemple" -> "202066",
    "JasonTerry" -> "1891",
    "IsaiahThomas" -> "202738",
    "LanceThomas" -> "202498",
    "HollisThompson" -> "203138",
    "JasonThompson" -> "201574",
    "KlayThompson" -> "202691",
    "TristanThompson" -> "202684",
    "MarcusThornton" -> "201977",
    "AnthonyTolliver" -> "201229",
    "AxelToupane" -> "1626253",
    "Karl-AnthonyTowns" -> "1626157",
    "PJTucker" -> "200782",
    "EvanTurner" -> "202323",
    "MylesTurner" -> "1626167",
    "BenoUdrih" -> "2757",
    "JonasValanciunas" -> "202685",
    "AndersonVarejao" -> "2760",
    "GreivisVasquez" -> "202349",
    "RashadVaughn" -> "1626173",
    "CharlieVillanueva" -> "101111",
    "NoahVonleh" -> "203943",
    "NikolaVucevic" -> "202696",
    "SashaVujacic" -> "2756",
    "DwyaneWade" -> "2548",
    "DionWaiters" -> "203079",
    "KembaWalker" -> "202689",
    "JohnWall" -> "202322",
    "TJWarren" -> "203933",
    "CJWatson" -> "201228",
    "BrianteWeber" -> "1627362",
    "SonnyWeems" -> "201603",
    "DavidWest" -> "2561",
    "RussellWestbrook" -> "201566",
    "HassanWhiteside" -> "202355",
    "ShayneWhittington" -> "203963",
    "AndrewWiggins" -> "203952",
    "CJWilcox" -> "203912",
    "AlanWilliams" -> "1626210",
    "DeronWilliams" -> "101114",
    "DerrickWilliams" -> "202682",
    "ElliotWilliams" -> "202343",
    "LouWilliams" -> "101150",
    "MarvinWilliams" -> "101107",
    "MoWilliams" -> "2590",
    "JustiseWinslow" -> "1626159",
    "JeffWithey" -> "203481",
    "ChristianWood" -> "1626174",
    "MettaWorldPeace" -> "1897",
    "BrandanWright" -> "201148",
    "DelonWright" -> "1626153",
    "DorellWright" -> "2748",
    "TonyWroten" -> "203100",
    "JamesYoung" -> "203923",
    "JoeYoung" -> "1626202",
    "NickYoung" -> "201156",
    "ThaddeusYoung" -> "201152",
    "CodyZeller" -> "203469",
    "TylerZeller" -> "203092"
  )

  val players = playerNameToId.keys.toArray

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
