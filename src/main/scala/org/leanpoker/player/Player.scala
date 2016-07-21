package org.leanpoker.player

import com.google.gson.JsonElement
import org.leanpoker.player.Poker.Deck

object Player {

  val VERSION = "Pokala never fold"

  val random = new scala.util.Random()
  def betRequest(request: JsonElement) = {
    val state = Parsing.parseGame(request)
    val bet = state.fold(0) { s =>
      val rem = Poker.cards diff s.myCombinedCards
      val rank = Poker.rank(s.myCombinedCards)
      val deck = Deck(rem)
      val preFlopOdds = Poker.odds(s.myHoleCards, List(), deck, s.players.size - 1, 100)
      val odds = Poker.odds(s.myHoleCards, s.communityCards, deck, s.players.size - 1, 100)
      val combinedOdds = (preFlopOdds._1 + odds._1) / 2.0
      val randomOdds = 1.0 / s.players.size
      if (combinedOdds == 0.0) 0
      else if (combinedOdds > randomOdds) {
        val raiseAmount = if (rank >= (Poker.ranks.size + 2 + 3)) {
          math.max(s.minimumRaiseAmount * 1.5, s.minimumRaiseAmount + 5)
        } else {
          s.minimumRaiseAmount + 5
        }
        math.min(raiseAmount.toInt, s.myStack)
      }
      else math.min(s.minimumCallAmountAggresive, s.myStack)
    }
    System.err.println(s"Our bet $bet")
    bet
  }

  def showdown(game: JsonElement) {

  }
}
