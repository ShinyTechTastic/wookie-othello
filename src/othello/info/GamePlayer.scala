package othello.info
import scala.util.Random

abstract class GamePlayer {
  def choose( list:List[(Int,Int)] , b:Board):(Int,Int)
}

object RandomPlayer extends GamePlayer {
  override def choose( list:List[(Int,Int)] , b:Board):(Int,Int) = Random.shuffle(list).head
}

abstract class ScoringPlayer extends GamePlayer {
  def score( move:(Int,Int) , b:Board ):Double
  
  def findBestMove( moves:List[(Int,Int)] , b:Board ):(Double,(Int,Int)) = {
  	val me = ( score(moves.head , b ) , moves.head )
  	if ( moves.tail.isEmpty ) me 
  	else{
  	  val best = findBestMove( moves.tail , b );
  	  if ( me._1 > best._1 ) best
  	  else me
  	}
  }  
  
  override def choose( list:List[(Int,Int)] , b:Board):(Int,Int) = findBestMove( list , b )._2
}

class FlipScoringPlayer(val rand:Double) extends ScoringPlayer {
  override def score( move:(Int,Int) , b:Board ):Double = b.getFlippedTiles( move ).length + (Random.nextDouble * rand)
}

class PrintPlayer(inner:GamePlayer,name:String) extends GamePlayer{
  override def choose( list:List[(Int,Int)] , b:Board):(Int,Int) = {
    println( name + " to Play")
    b.print
    val choice = inner.choose( list , b );
    println( choice )
    choice
  }
}