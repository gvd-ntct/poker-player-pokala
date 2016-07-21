package org.leanpoker.player

import com.google.gson.JsonElement

object Player {

  val VERSION = "Pokala never fold"
  
  val random = new scala.util.Random()
  def betRequest(request: JsonElement) = {
    val state = Parsing.parseGame(request)
    val bet = state.fold(0) { s =>
      val num = random.nextInt(10)
      if (num < 8) {
        math.min(s.minimumCallAmount, s.myStack)
      } else {
        math.min(s.minimumRaiseAmount + 5, s.myStack)
      }
    }
    System.err.println(s"Our bet $bet")
    bet
  }

  def showdown(game: JsonElement) {

  }
}
