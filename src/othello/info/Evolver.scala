package othello.info
import scala.util.Random

class Evolver[N]( val players:List[N] , compare:(N,N)=>(N,N), breed: (N,N)=>N ){
	
  def splitGroups( players:List[N] , accu:(List[(N,N,N,N)],List[N]) ):(List[(N,N,N,N)],List[N]) = {
    if ( players.length < 4 )
      accu
    else
      splitGroups( players.tail.tail.tail.tail , 
          ( (players.head , players.tail.head , players.tail.tail.head , players.tail.tail.tail.head ) :: accu._1 , accu._2) )
  }
  
  def compete( players:(N,N,N,N) ):List[N] = {
	val (a,b,c,d) = players;
    val (bestAB,worstAB) = compare(a,b) // ideally these would be in parallel 
    val (bestCD,worstCD) = compare(c,d)
   
    val newPlayer = breed( bestAB, bestCD)
    val (playOffWinner,playOffLoser) = compare( worstAB , worstCD )
    
    List( bestAB , bestCD , playOffWinner , compare(newPlayer,playOffLoser)._1 )
  }
  
  def generation: Evolver[N] = {
    val (groups,byes) = splitGroups( Random.shuffle(players) , (List(),List()) )
    val n = groups.par.flatMap( Timer.timeFunction("compare",compete) ) // do some paralisation here, should run each set of players seperatly
    new Evolver[N]( n.toList ::: byes , compare , breed )
  }
}

object Evovler {
  def compare[N <: GamePlayer](a:N,b:N):(N,N) = {
    val t = new Tournament(a,b)
    val (aScore,bScore) = t.bestOf(3)
    if ( aScore > bScore ) (a,b) else (b,a)
  }
}