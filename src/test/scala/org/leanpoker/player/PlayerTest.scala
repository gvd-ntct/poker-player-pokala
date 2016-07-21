package org.leanpoker.player

import com.google.gson.JsonParser
import org.scalatest.{FunSpec, MustMatchers}

class PlayerTest extends FunSpec with MustMatchers {

  it("test bet request") {
    val jsonElement = new JsonParser().parse("{\"key1\": \"value1\", \"key2\": \"value2\"}")
    Player.betRequest(jsonElement) must be (0)
  }
  it("test json") {
    val json = """{"tournament_id":"5740866147cecc0003000155","game_id":"5740ece2d9f98200030004d3","round":1, "bet_index":1, "in_action":1, "minimum_raise":20, "players":[{"name":"Smiling Monkey","stack":998,"status":"active","bet":2,"hole_cards":[{"rank":"7","suit":"hearts"},{"rank":"9","suit":"spades"}],"version":"Ekart, Laszlo","id":0},{"name":"All In","stack":996,"status":"active","bet":4,"hole_cards":[{"rank":"K","suit":"spades"},{"rank":"K","suit":"hearts"}],"version":"Objects in mirror are closer than they appear","id":1},{"name":"nodists","stack":0,"status":"out","bet":0,"hole_cards":[],"version":"It's a long way to the top - if you want to rock and roll!","id":2},{"name":"Fantastic Pony","stack":2000,"status":"folded","bet":0,"hole_cards":[{"rank":"3","suit":"diamonds"},{"rank":"Q","suit":"diamonds"}],"version":"Ludicrous poker player","id":3}],"small_blind":2,"big_blind":4,"orbits":0,"dealer":3,"community_cards":[],"current_buy_in":4,"pot":6}"""
    val jsonElement = new JsonParser().parse(json)
    val result = Parsing.parseGame(jsonElement)
    result must not be None
    println(result)
  }
}