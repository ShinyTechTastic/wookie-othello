package othello.info

class GeneticPlayer ( val data:List[Double] ) extends Function2[(Int,Int),Board,Double] {

  def apply( pos:(Int,Int) , board:Board):Double = {
   val newBoard = board.play( pos )
   val allScores:List[Double] = Board.allMoves.map(  // allMoves is also all positions
       pos => data( posIndex(pos, newBoard.at(pos)) ) );
   allScores.foldRight( 0.0 )( (a,b)=>a+b )
  }
  
  def posIndex( pos:(Int,Int) , player:Player ):Int = {
    if ( player == Black ) posIndex2( pos , 1 )
    else if ( player == White ) posIndex2( pos , -1 )
    posIndex2( pos , 0 );
  }
  
  def posIndex2( pos:(Int,Int) , offset:Int ):Int = {
    val (x,y) = pos
    x + (8*y) + 64 * offset
  }
}