package othello.info

abstract class MiniMax( val depth:Int ) extends GamePlayer {
  
  def maxi (a:Double,b:Double):Boolean = a<b
  def mini (a:Double,b:Double):Boolean = a>b
  
  def scoreFunction(p:Player):Move=>Double
  
  def scorer( useMaxi:Boolean ):(Double,Double)=> Boolean = if (useMaxi) maxi else mini
  
  def findBestMove( moves:List[(Double,Move)] , compare:(Double,Double)=>Boolean ):(Double,Move) = {
    if ( moves.tail.isEmpty ) 
      moves.head
    else{
      val other = findBestMove( moves.tail , compare )
      if ( compare(moves.head._1 , other._1 ) )
        moves.head
      else
        other
    }
  }
  
  override def choose( list:List[Move] , b:Board):(Move) = {
    
    val score = scoreFunction( b.turn );
    
	  def scoreList( list:List[Move] ):List[(Double,Move)] = {
	    list.map( a => (score(a),a) )
	  } 
	  
	  def searchList( list:List[Move] , depth:Int , useMaxi:Boolean ):List[(Double,Move)] = {
	    list.map( a => {
	      val (score,_) = doMiniMax( a.board.validMoves , a.board , depth , useMaxi ) // we don't care what the next move is
	      ( score , a )
	    });
	  }
	  
	  
	  def doMiniMax( list:List[Move] , board:Board , depth:Int , useMaxi:Boolean ):(Double,(Move)) = {
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