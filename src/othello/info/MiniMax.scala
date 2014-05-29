package othello.info

import scala.collection.parallel.immutable.ParSeq

abstract class MiniMax( val depth:Int ) extends GamePlayer {
  
  def maxi (a:Double,b:Double):Boolean = a<b
  def mini (a:Double,b:Double):Boolean = a>b
  
  def scoreFunction(p:Player):Move=>Double
  
  def scorer( useMaxi:Boolean ):(Double,Double)=> Boolean = if (useMaxi) maxi else mini
  
  def findBestMove( moves:ParSeq[(Double,Move)] , compare:(Double,Double)=>Boolean ):(Double,Move) = {
    def findBest( a:(Double,Move) , b:(Double,Move) ):(Double,Move) = {
      if ( compare(a._1,b._1) ) a else b
    }
    moves.fold( (Double.MinValue,Move.pass) )( findBest )
  }
  
  override def choose( list:ParSeq[Move] , b:Board):(Move) = {
    
    val score = scoreFunction( b.turn );
    
	  def scoreList( list:ParSeq[Move] ):ParSeq[(Double,Move)] = {
	    list.map( a => (score(a),a) )
	  } 
	  
	  def searchList( list:ParSeq[Move] , depth:Int , useMaxi:Boolean ):ParSeq[(Double,Move)] = {
	    list.map( a => {
	      val (score,_) = doMiniMax( a.board.validMoves , a.board , depth , useMaxi ) // we don't care what the next move is
	      ( score , a )
	    });
	  }
	  
	  def doMiniMax( list:ParSeq[Move] , board:Board , depth:Int , useMaxi:Boolean ):(Double,(Move)) = {
	    if ( list.isEmpty )
	      if (useMaxi) (Double.MaxValue,Move.pass) else (Double.MinValue,Move.pass) // pass moves?
	    else if ( depth <= 0 ){
	      // flat scoring...
	      findBestMove( scoreList(list) , scorer(useMaxi) )
	    }else{
	      // recurse inwards
	      findBestMove( searchList(list, depth-1 , !useMaxi ) , scorer(useMaxi) )
	    }
	  }
    
    doMiniMax( list , b , depth , true )._2
  }
}

object MiniMax{
  // sample scoring function
  def tileCount(player:Player) =
    if ( player == Black ){
      scoreBlackTiles _
    }else{
      scoreWhiteTiles _
    }
 
  def scoreBlackTiles( pos:(Int,Int),board:Board):Double = {
    val count = board.play(pos).count;
    count._1 - count._2
  }
  
  def scoreWhiteTiles( pos:(Int,Int),board:Board):Double = {
    val count = board.play(pos).count;
    count._2 - count._1
  }
}