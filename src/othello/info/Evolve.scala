package othello.info
import scala.util.Random
import java.util.Calendar

object Evolve extends App {
	
	override def main(args: Array[String]) {
	  val r = new Random()
	  val initial = generateInitialPlayers( 20 , r);
	  
	  val time = Calendar.getInstance().getTime()
	  println(time + " starting the games" );
	  
	  val finalPlayers = doEvolveLoops( initial , 5 );
	  println("Final Stats...")
	  generationStats( initial , finalPlayers )
	}
  
	def generateInitialPlayers( n:Int , r:Random ):List[GeneticPlayer] = {
	  def innerGenerate( accu:List[GeneticPlayer] , n:Int ):List[GeneticPlayer] = 
	    if ( n == 0 ) accu
	    else innerGenerate( GeneticPlayer.randomPlayer(r) :: accu , n-1 )
	  innerGenerate( List() , n )
	}
	
	def generationStats( origPlayers:List[GeneticPlayer] , newPlayers:List[GeneticPlayer] ){
	  def counters( orig:List[GeneticPlayer] , acum:(Int,Int) ):(Int,Int) =
	    if ( orig.isEmpty ) acum
	    else counters( orig.tail , if ( newPlayers.contains(orig.head) ) (acum._1+1,acum._2) else (acum._1,acum._2+1) )
	  val (same,diff) = counters( origPlayers , (0,0) )
	  println(Calendar.getInstance().getTime() + "Same "+same+"  Different "+diff );
	}
	
	def doEvolveLoops( players:List[GeneticPlayer] , n:Int ):List[GeneticPlayer] = {
	  if ( n > 0 ){
		  val e = new Evolver( players , Evovler.compare[GeneticPlayer] , GeneticPlayer.combine )
		  val k = e.generation
		  generationStats( players , k.players )
		  doEvolveLoops( k.players , n-1 )
	  }else{
	    players
	  }
	}
}