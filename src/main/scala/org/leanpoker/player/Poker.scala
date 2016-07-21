package org.leanpoker.player

import com.google.gson.{JsonArray, JsonElement}
import org.leanpoker.player.Poker.Deck

import scala.util.{Random, Try}

/**
  * Created by ghvandoorn on 7/21/16.
  */
object Poker {
  sealed trait Rank
  case object Ace extends Rank
  case object King extends Rank
  case object Queen extends Rank
  case object Jack extends Rank
  case object Two extends Rank
  case object Three extends Rank
  case object Four extends Rank
  case object Five extends Rank
  case object Six extends Rank
  case object Seven extends Rank
  case object Eight extends Rank
  case object Nine extends Rank
  case object Ten extends Rank

  // Specify in order
  val ranks = List(Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace)

  sealed trait Suit
  case object Hearts extends Suit
  case object Diamonds extends Suit
  case object Clubs extends Suit
  case object Spades extends Suit

  // Just an enumeration
  val suits = List(Hearts, Diamonds, Clubs, Spades)

  case class Card(suit: Suit, rank: Rank)

  // Parse into a card
  object Card {
    def parse(suitString: String, rankString: String): Option[Card] = {
      val suit = suitString match {
        case "spades" => Some(Spades)
        case "clubs" => Some(Clubs)
        case "hearts" => Some(Hearts)
        case "diamonds" => Some(Diamonds)
        case _ => None
      }
      val rank = rankString match {
        case "2" => Some(Two)
        case "3" => Some(Three)
        case "4" => Some(Four)
        case "5" => Some(Five)
        case "6" => Some(Six)
        case "7" => Some(Seven)
        case "8" => Some(Eight)
        case "9" => Some(Nine)
        case "10" => Some(Ten)
        case "J" => Some(Jack)
        case "Q" => Some(Queen)
        case "K" => Some(King)
        case "A" => Some(Ace)
        case _ => None
      }
      for {
        r <- rank
        s <- suit
      } yield Card(s, r)
    }
  }


  case class Deck(cards: Cards) {
    def shuffle = Deck(Random.shuffle(cards))
    def take(n: Int): (Cards, Deck) = (cards.take(n), Deck(cards.drop(n)))
  }

  type Cards = List[Card]

  val cards = for {
    suit <- suits
    rank <- ranks
  } yield Card(suit, rank)

  val deck = Deck(cards)

  case class Player(id: Int, name: String, status: String, version: String, stack: Int, bet: Int, holeCards: Cards)
  case class GameState(tournamentId: String,
                       gameId: String,
                       round: Int,
                       betIndex: Int,
                       minimumRaise: Int,
                       inAction: Int,
                       smallBlind: Int,
                       orbits: Int,
                       dealer: Int,
                       currentBuyIn: Int,
                       pot: Int,
                       players: IndexedSeq[Player],
                       communityCards: Cards) {
    def minimumCallAmount: Int = math.max(0, currentBuyIn - players(inAction).bet)
    def minimumCallAmountAggresive: Int = {
      if ( ((dealer+1) % players.size) == inAction) {
        math.min(10, myStack)
      } else minimumCallAmount
    }
    def minimumRaiseAmount: Int = math.max(0, minimumCallAmount + minimumRaise)
    def myHoleCards = players(inAction).holeCards
    def myCombinedCards = myHoleCards ++ communityCards
    def myStack = players(inAction).stack
  }
}

object Parsing {
  import Poker._
  def parseCards(cards: JsonArray) = {
    (0 to cards.size - 1).map(j => cards.get(j).getAsJsonObject).flatMap { c => Poker.Card.parse(c.get("suit").getAsString, c.get("rank").getAsString)}.toList
  }
  def parsePlayers(players: JsonArray) = {
    (0 to players.size - 1).flatMap { i =>
      val playerJson = players.get(i).getAsJsonObject
      for {
        id <- Try(playerJson.get("id").getAsInt).toOption
        name <- Try(playerJson.get("name").getAsString).toOption
        status <- Try(playerJson.get("status").getAsString).toOption
        version <- Try(playerJson.get("version").getAsString).toOption
        stack <- Try(playerJson.get("stack").getAsInt).toOption
        bet <- Try(playerJson.get("bet").getAsInt).toOption
        cards = Try(playerJson.get("hole_cards").getAsJsonArray).toOption.map(parseCards).getOrElse(List())
      } yield Poker.Player(id, name, status, version, stack, bet, cards)
    }
  }

  def parseGame(state: JsonElement): Option[GameState] = {
    val obj = state.getAsJsonObject
    for {
      tournamentId <- Try(obj.get("tournament_id").getAsString).toOption
      gameId <- Try(obj.get("game_id").getAsString).toOption
      round <- Try(obj.get("round").getAsInt).toOption
      betIndex <- Try(obj.get("bet_index").getAsInt).toOption
      minimumRaise <- Try(obj.get("minimum_raise").getAsInt).toOption
      inAction <- Try(obj.get("in_action").getAsInt).toOption
      smallBlind <- Try(obj.get("small_blind").getAsInt).toOption
      orbits <- Try(obj.get("orbits").getAsInt).toOption
      dealer <- Try(obj.get("dealer").getAsInt).toOption
      currentBuyIn <- Try(obj.get("current_buy_in").getAsInt).toOption
      pot <- Try(obj.get("pot").getAsInt).toOption
      communityCards <- Try(obj.get("community_cards").getAsJsonArray).toOption.map(parseCards)
      players <- Try(obj.get("players").getAsJsonArray).toOption.map(parsePlayers)
    } yield GameState(tournamentId, gameId, round, betIndex, minimumRaise,
      inAction, smallBlind, orbits, dealer, currentBuyIn, pot, players, communityCards)
  }
}
