package othello.info
import scala.util.Random
import scala.collection.parallel.immutable.ParSeq

abstract class GamePlayer {
  def choose( list:ParSeq[Move] , b:Board ):(Move)
}

object RandomPlayer extends GamePlayer {
  override def choose( list:ParSeq[Move] , b:Board ):(Move) = Random.shuffle(list.toList).head
}

abstract class ScoringPlayer extends GamePlayer {
  def score( move:(Move) , b:Board ):Double
  
  def findBestMove( moves:ParSeq[Move] , b:Board ):(Double,(Move)) = {
    def findBest( a:(Double,Move) , b:(Double,Move) ):(Double,Move) = {
      if ( a._1 > b._1 ) a else b
    }
    moves.map( x=> (score(x,b) , x) ).reduce( findBest )
  }  
  
  override def choose( list:ParSeq[Move] , b:Board):(Move) = findBestMove( list , b )._2
}

class FlipScoringPlayer(val rand:Double) extends ScoringPlayer {
  override def score( move:(Move) , b:Board ):Double = b.getFlippedTiles( move.pos ).length + (Random.nextDouble * rand)
}

class PrintPlayer(inner:GamePlayer,name:String) extends GamePlayer{
  override def choose( list:ParSeq[Move] , b:Board):(Move) = {
    println( name + " to Play")
    b.print
    val choice = inner.choose( list , b );
    println( choice )
    choice
  }
}