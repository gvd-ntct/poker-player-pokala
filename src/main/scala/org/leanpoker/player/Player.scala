package org.leanpoker.player

import com.google.gson.JsonElement

object Player {

  val VERSION = "Pokala initial data model"
  val random = new scala.util.Random()
  def betRequest(request: JsonElement) = {
    val state = Parsing.parseGame(request)
    val bet = state.fold(0) { s =>
      val num = random.nextInt(10)
      if (num < 4) {
        math.min(s.minimumCallAmount, s.myStack)
      } else if (num == 0) {
        math.min(s.minimumRaiseAmount + 2, s.myStack)
      } else 0
    }
    System.err.println(s"Our bet $bet")
    bet
  }

  def showdown(game: JsonElement) {

  }
}
