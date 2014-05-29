package othello.info
import scala.util.Random

/**
 * This class evolves a set of players. It needs two functions - compare which compares two players 
 *  and returns them as a pair (best,worst) and breed which takes to players and produces a hybrid.
 * The function generation returns a new evovler after the work is done.
 * 
 * Works by creating groups of four players (e.g. to pairs) the pairs play each other and the winners
 *  are preserverd and breed, the lossers compete in a play off and the worst player competes with the breed player.
 *  if the new player wins it and the playoff winner goes through if not the original set are preserved.
 */

class Evolver[N]( val players:List[N] , compare:(N,N)=>(N,N), breed: (N,N)=>N ){
	
  /**
   * Creates groups of four players, and a set of players that got buys.
   */
  def splitGroups( players:List[N] , accu:(List[(N,N,N,N)],List[N]) ):(List[(N,N,N,N)],List[N]) = {
    if ( players.length < 4 )
      accu
    else
      splitGroups( players.tail.tail.tail.tail , 
          ( (players.head , players.tail.head , players.tail.tail.head , players.tail.tail.tail.head ) :: accu._1 , accu._2) )
  }
  
  /**
   * Takes a set of four players and competes them.
   */
  def compete( players:(N,N,N,N) ):List[N] = {
	val (a,b,c,d) = players;
    val (bestAB,worstAB) = compare(a,b) // ideally these would be in parallel 
    val (bestCD,worstCD) = compare(c,d)
   
    val newPlayer = breed( bestAB, bestCD)
    val (playOffWinner,playOffLoser) = compare( worstAB , worstCD )
    
    List( bestAB , bestCD , playOffWinner , compare(newPlayer,playOffLoser)._1 )
  }
  
  /**
   * Runs the next genratation
   */
  def generation: Evolver[N] = {
    val (groups,byes) = splitGroups( Random.shuffle(players) , (List(),List()) )
    val n = groups.par.flatMap( Timer.timeFunction("compete",compete) ) // do some paralisation here, should run each set of players seperatly
    new Evolver[N]( n.toList ::: byes , compare , breed )
  }
}

object Evovler {
  /**
   * Compare function for extensions GamePlayers, uses the Tournamnet as a best of 3
   */
  def compare[N <: GamePlayer](a:N,b:N):(N,N) = {
    val t = new Tournament(a,b)
    val (aScore,bScore) = t.bestOf(3)
    if ( aScore > bScore ) (a,b) else (b,a)
  }
}