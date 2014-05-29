package othello.info
import scala.util.Random

class GeneticPlayer( val data:List[Double] ) extends MiniMax( 3 ) { 
  def scoreFunction(p:Player):((Int,Int),Board)=>Double = new GeneticChooser( data )(p)
}

class GeneticChooser( val data:List[Double] ) extends Function1[Player, ((Int,Int),Board) => Double] {

  def apply( p:Player ):((Int,Int),Board) => Double = {
    if ( p == Black )
      evalBlack _
    else
      evalWhite _
  }
  
  def evalBlack( pos:(Int,Int) , board:Board):Double = {
   val newBoard = board.play( pos )
   val allScores:List[Double] = Board.allMoves.map(  // allMoves is also all positions
       pos => data( posIndex(pos, newBoard ) ) );
   allScores.foldRight( 0.0 )( (a,b)=>a+b )
  }
  
  def evalWhite( pos:(Int,Int) , board:Board):Double = - evalBlack( pos , board )
  
  final def offset( p:Player):Int = {
    if ( p == Black ) 1
    else if ( p == White ) 2
    else 0
  }
  
  final def findCorner( pos:(Int,Int) ):(Int,Int) = {
    def findCorner1(a:Int) = if (a >= 4 ) 7 else 0;
    val (x,y) = pos
    ( findCorner1(x) , findCorner1(y) )
  }
  
  def posIndex( pos:(Int,Int) , board:Board ):Int = {
    def corner = findCorner(pos);
    
    def player = board.at(pos);
    def cornerPlayer = board.at(corner);
    
    posIndex2( pos , 2*offset(player) + offset(cornerPlayer) );
  }
  
  def symetry(a:Int):Int = if (a > 4 ) 8-a else a
  
  def posIndex2( pos:(Int,Int) , offset:Int ):Int = {
    val (x,y) = pos
    val (x2,y2) = ( symetry(x),symetry(y) )
    (x + (4*y)) + (16 * offset)
  }
}

object GeneticPlayer {
  
  def combine( a:GeneticPlayer , b:GeneticPlayer ):GeneticPlayer = {
    new GeneticPlayer( combineData(a.data,b.data ) )
  }
  
  def combineData( a:List[Double] , b:List[Double] ):List[Double] = {
    def combine( a:List[Double] , b:List[Double] , accu:List[Double]):List[Double] =
      if ( a.isEmpty ) accu
      else if ( Random.nextBoolean ) 
        combine( a.tail , b.tail , a.head :: accu )
      else combine( a.tail , b.tail , b.head :: accu )
    combine( a , b , List() )
  }
  
  def randomPlayer( r:Random ):GeneticPlayer = {
    def inner( accu:List[Double] ):GeneticPlayer =
      if ( accu.length == (16*3*3) )
        new GeneticPlayer( accu )
      else inner( Random.nextDouble :: accu );
    inner( List() )
  }
}