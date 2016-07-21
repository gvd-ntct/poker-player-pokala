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

  def value(rank: Rank) = ranks.indexOf(rank)
  def value(suit: Suit) = suits.indexOf(suit)
  def value(card: Card) = suits.indexOf(card.rank)

  implicit val rankOrdering = new Ordering[Rank] {
    override def compare(x: Rank, y: Rank): Int = {
      value(x) - value(y) // Not the fastest but concise
    }
  }
  implicit val suitOrdering = new Ordering[Suit] {
    override def compare(x: Suit, y: Suit): Int = {
      value(x) - value(y)
    }
  }
  implicit val cardByRankOrdering: Ordering[Card] = Ordering.by(_.rank)

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

  case class MatchedAndRest(matches: Cards, rem: Cards)
  // Identical ranks
  object IsFourOfAKind {
    def unapply(cards: Cards): Option[MatchedAndRest] = {
      cards.groupBy(_.rank).find(_._2.size == 4).map { case (rank, matches) =>
        MatchedAndRest(matches, cards diff matches)
      }
    }
  }
  object IsThreeOfAKind {
    def unapply(cards: Cards): Option[MatchedAndRest] = {
      cards.groupBy(_.rank).find(_._2.size == 3).map { case (rank, matches) =>
        MatchedAndRest(matches, cards diff matches)
      }
    }
  }
  object IsPair {
    def unapply(cards: Cards): Option[MatchedAndRest] = {
      cards.groupBy(_.rank).find(_._2.size == 2).map { case (rank, matches) =>
        MatchedAndRest(matches, cards diff matches)
      }
    }
  }

  object IsTwoPair {
    def unapply(cards: Cards): Option[MatchedAndRest] = {
      val groups = cards.groupBy(_.rank).filter(_._2.size == 2).toList.map(_._2).sortBy(_.head).reverse
      if (groups.size >= 2) {
        val matches = groups.take(2).flatten
        Some(MatchedAndRest(matches, cards diff matches))
      } else None
    }
  }

  object IsStraightFlush {
    def unapply(cards: Cards): Option[MatchedAndRest] = {
      cards match {
        case IsStraight(matches) if matches.matches.groupBy(_.suit).head._2.size == matches.matches.size => Some(matches)
        case _ => None
      }
    }
  }

  object IsStraight {
    def unapply(cards: Cards): Option[MatchedAndRest] = {
      cards.sorted.sliding(5).flatMap {l =>
        val s = l.map(_.rank).toSet
        if (l.size == s.size && (value(l.max.rank) - value(l.min.rank)) == 4) Some(l)
        else None
      }.toList.headOption.map(m => MatchedAndRest(m, cards diff m))
    }
  }

  object IsFlush {
    def unapply(cards: Cards): Option[MatchedAndRest] = {
      cards.groupBy(_.suit).filter(_._2.size >= 5).toList.map(_._2.sorted.reverse.take(5))
        .headOption.map { matches => MatchedAndRest(matches, cards diff matches) }
    }
  }

  object IsRoyalFlush {
    def unapply(cards: Cards): Option[MatchedAndRest] = cards match {
      case IsFlush(matches) if matches.matches.sorted.reverse.head.rank == Ace => Some(matches)
      case _ => None
    }
  }
  object IsHighCard {
    def unapply(cards: Cards): Option[MatchedAndRest] = {
      val matches = List(cards.max)
      Some(MatchedAndRest(matches, cards diff matches))
    }
  }

  object IsFullHouse {
    def unapply(cards: Cards): Option[MatchedAndRest] = None
  }

  def rank(cards: Cards): Int = cards match {
    case IsRoyalFlush(matches) => ranks.size + 2 + 9
    case IsStraightFlush(matches) => ranks.size + 2 + 8
    case IsFourOfAKind(matches) => ranks.size + 2 + 7
    case IsFullHouse(matches) => ranks.size + 2 + 6
    case IsFlush(matches) => ranks.size + 2 + 5
    case IsStraight(matches) => ranks.size + 2 + 4
    case IsThreeOfAKind(matches) => ranks.size + 2 + 3
    case IsTwoPair(matches) => ranks.size + 2 + 2
    case IsPair(matches) => ranks.size + 2 + 1
    case IsHighCard(matches) => ranks.indexOf(matches.matches.head.rank) + 2
    case _ => 0
  }

  /**
    * This method currently incorrectly handles equal hands (e.g. Two Pairs vs Two Pairs)
    *
    * @param hole
    * @param community
    * @param remaining
    * @param players
    * @param iterations
    * @return
    */
  def odds(hole: Cards, community: Cards, remaining: Deck, players: Int, iterations: Int) = {
    val combined = hole ++ community
    val myRank = rank(combined)
    val games = 1 to iterations map { iter =>
      var current = remaining.shuffle // Play different games
    val playerRanks = 1 to players map { player =>
        val (playerCards, newDeck) = current.take(2)
        current = newDeck
        val playerCombined = playerCards ++ community
        val playerRank = rank(playerCombined)
        if (myRank > playerRank) true
        else if (myRank == playerRank && value(combined.max) > value(playerCombined.max)) true
        else false
      }
      playerRanks.forall(identity)
    }
    val wins = games.count(identity)
    val losses = games.size - wins
    (wins.toDouble / iterations, losses.toDouble / iterations)
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
