package othello.info

abstract class MiniMax( val depth:Int ) extends GamePlayer {
  
  def maxi (a:Double,b:Double):Boolean = a<b
  def mini (a:Double,b:Double):Boolean = a>b
  
  def scoreFunction(p:Player):((Int,Int),Board)=>Double
  
  def scorer( useMaxi:Boolean ):(Double,Double)=> Boolean = if (useMaxi) maxi else mini
  
  def findBestMove( moves:List[(Double,(Int,Int))] , compare:(Double,Double)=>Boolean ):(Double,(Int,Int)) = {
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
  
  override def choose( list:List[(Int,Int)] , b:Board):(Int,Int) = {
    
    val score = scoreFunction( b.turn );
    
	  def scoreList( list:List[(Int,Int)] , board:Board ):List[(Double,(Int,Int))] = {
	    list.map( a => (score(a,board),a) )
	  } 
	  
	  def searchList( list:List[(Int,Int)]  , board:Board , depth:Int , useMaxi:Boolean ):List[(Double,(Int,Int))] = {
	    list.map( a => {
	      val newBoard = board.play(a)
	      val (score,_) = doMiniMax( newBoard.validMoves , newBoard , depth , useMaxi ) // we don't care what the next move is
	      ( score , a )
	    });
	  }
	  
	  
	  def doMiniMax( list:List[(Int,Int)] , board:Board , depth:Int , useMaxi:Boolean ):(Double,(Int,Int)) = {
	    if ( list.isEmpty )
	      if (useMaxi) (Double.MaxValue,(-1,-1)) else (Double.MinValue,(-1,-1))
	    else if ( depth <= 0 ){
	      // flat scoring...
	      findBestMove( scoreList(list,board) , scorer(useMaxi) )
	    }else{
	      // recurse inwards
	      findBestMove( searchList(list,board,depth-1 , !useMaxi ) , scorer(useMaxi) )
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