package othello.info
import scala.util.Random

class Tournament( val p1:GamePlayer , val p2:GamePlayer) {

  def playMatch( pA:GamePlayer , pB:GamePlayer ):Int = {
    // returns 1 if pA won, 2 if pB won or -1 if it's draw
    def playMove( b:Board ):Board = {
      val moves = b.validMoves
		if ( moves.isEmpty ) b // base case, no moves avaliable
		else{
		  if ( b.turn == Black )
			playMove( pA.choose( moves , b ).board )
		  else
			playMove( pB.choose( moves , b ).board )
		}
    }
    val b = playMove( Board.initial )
    val ( p1score , p2score ) = b.count
    
    if ( p1score > p2score ) 1
    else if ( p1score < p2score ) 2
    else -1
  }
  
  def playRound():Int = {
    val tPlayMatch = Timer.timeFunction("playMatch", playMatch _ )
    if ( Random.nextBoolean ){
      tPlayMatch( p1 , p2 )
    }else{
      val n = tPlayMatch( p2 , p1 )
      if ( n == 1 ) 2
      else if ( n==2 ) 1
      else -1
    }
  }
  
  def bestOf( n:Int ):(Int,Int) = {
    def bestOfInner( accu:(Int,Int) ):(Int,Int) = {
      if ( accu._1 >= n || accu._2 >= n ) accu // base case
      else {
        val winner = playRound()
        if ( winner == 1 )
          bestOfInner( (accu._1 +1 , accu._2 ) )
        else if ( winner == 2 )
          bestOfInner( (accu._1 , accu._2 +1 ) )
        else
          bestOfInner( (accu._1 , accu._2 ) )
      }
    }
    Timer.timeFunction("bestOfInner", bestOfInner _ )( (0,0) );
  }
}