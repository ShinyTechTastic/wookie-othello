package othello.info

trait Player {
  def name:String
  def next:Player
  def display:Char
}
case object Black extends Player{
  def name = "Black"
  def next = White
  def display = '@'
}
case object White extends Player{
  def name = "White"
  def next = Black
  def display = 'O'
}
case object Empty extends Player{
  def name = "Empty"
  def next = throw new Error("Empty.next")
  def display = '.'
}

class Board ( val black: Long , val white:Long , val turn:Player ){

  def at( pos:(Int,Int) ): Player = {
    if ( Board.valid(pos) ) Empty
  	if ( Board.at(black,pos) ) Black 
  	else if ( Board.at(white,pos) ) White
  	else Empty
  }
  
  def getFlippedTiles( pos:(Int,Int) ) = Board.directions
    	.filter( x=> this.wouldFlip(turn,pos,x,0) > 0 ) // check we can flip these
    	.flatMap( x=> this.flipPositions( turn , pos , x ) ) // get the locations
  
  def play( pos:(Int,Int) ):Board = {
    val flips = this.getFlippedTiles( pos )
    if (turn == Black )
	  new Board( Board.placeMultipleStones(flips,black) , 
			  	 Board.unplaceMultipleStones(flips,white) , White )
	else
	  new Board( Board.unplaceMultipleStones(flips,black) , 
			  	 Board.placeMultipleStones(flips,white) , Black )
  }
  
  def validMoves():List[(Int,Int)] = Board.allMoves.filter( this.isValidMove )
    
  def isValidMove( pos:(Int,Int) ):Boolean = {
    // it's empty        AND             there exists a direction that would Flip something...
    ( at( pos ) == Empty ) && Board.directions.exists( x => this.wouldFlip( turn , pos , x , 0 ) > 0 )
  }
  
  def wouldFlip( turn:Player , pos:(Int,Int) , direction:(Int,Int) , steps:Int ):Int = {
	  val (pX,pY) = pos;
	  val (dX,dY) = direction;
	  val piece = this.at( pX+dX , pY+dY );
	  if ( piece == Empty ) 0 // nothing can flip here, even though we've looked..
	  else if ( piece == turn ) steps // we can flip to this location, which may be 0..
	  else wouldFlip( turn , (pX+dX,pY+dY) , direction , steps + 1) // loks positive, keep looking
  }
  
  def flipPositions( turn:Player , pos:(Int,Int) , direction:(Int,Int) ):List[(Int,Int)] = {
	  val (pX,pY) = pos;
	  val (dX,dY) = direction;
	  val piece = this.at( pX+dX , pY+dY );
	  if ( piece == Empty ) throw new Error("Empty in flipPositions!")
	  else if ( piece == turn ) List( pos ) // base case
	  else pos :: flipPositions( turn , (pX+dX,pY+dY) , direction)
  }
  
  def count():(Int,Int) = {
     def innerCount( list:List[(Int,Int)] , accu:(Int,Int) ):(Int,Int) = {
       if ( list.isEmpty ) accu
       else {
         val tile = this.at(list.head)
         if ( tile == Black ){
           innerCount( list.tail , ( accu._1 + 1 , accu._2 ) );
         }else if ( tile == White ){
           innerCount( list.tail , ( accu._1 , accu._2 + 1) );
         }else{
           innerCount( list.tail , accu );
         }
       }
     }
     innerCount( Board.allMoves , (0,0) );
  } 
  
  def print():Unit = {
     var x:Int = 0;
     var y:Int = 0;
      for( x <- 0 to 7){
        for( y <- 0 to 7) {
          Console.print( this.at(x,y).display )
        }
        println( "" )
      }
  } 
}

object Board {
  def offset( pos:(Int,Int) ) = (pos._1 + (pos._2*8))
  def at( data:Long , pos:(Int,Int) ): Boolean = (data >> offset(pos) & 0x1) == 1;
  def valid( pos:(Int,Int) ): Boolean = {
    val (x,y) = pos
    x >=0 && y >=0 && x<=7 && y<=7 
  }
  def placeStone( pos:(Int,Int) , data:Long ) = (0x1L << offset(pos)) | data
  def unplaceStone( pos:(Int,Int) , data:Long ) = (Long.MaxValue - (0x1L << offset(pos))) & data
  
  def placeMultipleStones( pos:List[(Int,Int)] , data:Long ): Long =
    if ( pos.isEmpty ) data 
    else placeMultipleStones( pos.tail , placeStone( pos.head , data ) )
  def unplaceMultipleStones( pos:List[(Int,Int)] , data:Long ): Long =
    if ( pos.isEmpty ) data 
    else unplaceMultipleStones( pos.tail , unplaceStone( pos.head , data ) )
  
  
  val allMoves:List[(Int,Int)] = allMovesAfter(0,0)
  
  // Recursivly generate the list of all possible moves
  def allMovesAfter( x:Int , y:Int ):List[(Int,Int)] =
    if ( x == 7 )
      if ( y == 7 ) List((x,y))
      else (x,y) :: allMovesAfter( 0 , y+1 );
    else (x,y) :: allMovesAfter( x+1 , y )
    
  val initalBlack = placeStone( (3,3) , placeStone( (4,4) , 0 ) )
  val initalWhite = placeStone( (3,4) , placeStone( (4,3) , 0 ) )
    
  val initial = new Board(initalBlack,initalWhite,Black) // this sets up the starting board
  
  val directions:List[(Int,Int)] = 
    (0,-1) :: (0,1) :: (-1,0) :: (1,0 ) ::
    (-1,-1) :: (1,1) :: (-1,1) :: (1,-1 ) :: List()
    
}