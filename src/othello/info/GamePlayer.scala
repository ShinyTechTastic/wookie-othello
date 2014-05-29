package othello.info
import scala.util.Random

abstract class GamePlayer {
  def choose( list:List[Move] , b:Board ):(Move)
}

object RandomPlayer extends GamePlayer {
  override def choose( list:List[Move] , b:Board ):(Move) = Random.shuffle(list).head
}

abstract class ScoringPlayer extends GamePlayer {
  def score( move:(Move) , b:Board ):Double
  
  def findBestMove( moves:List[Move] , b:Board ):(Double,(Move)) = {
  	val me = ( score(moves.head , b ) , moves.head )
  	if ( moves.tail.isEmpty ) me 
  	else{
  	  val best = findBestMove( moves.tail , b );
  	  if ( me._1 > best._1 ) best
  	  else me
  	}
  }  
  
  override def choose( list:List[Move] , b:Board):(Move) = findBestMove( list , b )._2
}

class FlipScoringPlayer(val rand:Double) extends ScoringPlayer {
  override def score( move:(Move) , b:Board ):Double = b.getFlippedTiles( move.pos ).length + (Random.nextDouble * rand)
}

class PrintPlayer(inner:GamePlayer,name:String) extends GamePlayer{
  override def choose( list:List[Move] , b:Board):(Move) = {
    println( name + " to Play")
    b.print
    val choice = inner.choose( list , b );
    println( choice )
    choice
  }
}