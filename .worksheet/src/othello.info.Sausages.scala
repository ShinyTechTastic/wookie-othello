package othello.info
import scala.util.Random

object Sausages {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(118); 
	
	val mmplayer = new MiniMax(2 , MiniMax.tileCount );System.out.println("""mmplayer  : othello.info.MiniMax = """ + $show(mmplayer ));$skip(61); val res$0 = 
	mmplayer.choose( Board.initial.validMoves , Board.initial );System.out.println("""res0: (Int, Int) = """ + $show(res$0));$skip(68); val res$1 = 
                   
	MiniMax.tileCount(Black)((3,5), Board.initial);System.out.println("""res1: Double = """ + $show(res$1));$skip(48); val res$2 = 
	MiniMax.tileCount(White)((3,5), Board.initial);System.out.println("""res2: Double = """ + $show(res$2));$skip(155); 

//	val t4 = new Tournament( new PrintPlayer(mmplayer,"mm") , new PrintPlayer(RandomPlayer,"random") )
	val t4 = new Tournament( mmplayer , RandomPlayer );System.out.println("""t4  : othello.info.Tournament = """ + $show(t4 ));$skip(37); val res$3 = 
                    

  t4.playRound;System.out.println("""res3: Int = """ + $show(res$3));$skip(128); 
  //t4.bestOf(50)
                           
                           
  val rp = GeneticPlayer.randomPlayer( new Random() );System.out.println("""rp  : othello.info.GeneticPlayer = """ + $show(rp ));$skip(61); 
                      
 	val rpplayer = new MiniMax(2 , rp );System.out.println("""rpplayer  : othello.info.MiniMax = """ + $show(rpplayer ));$skip(61); val res$4 = 
	rpplayer.choose( Board.initial.validMoves , Board.initial );System.out.println("""res4: (Int, Int) = """ + $show(res$4));$skip(91); 
                                     
 	val t5 = new Tournament( rpplayer , RandomPlayer );System.out.println("""t5  : othello.info.Tournament = """ + $show(t5 ));$skip(17); val res$5 = 
 
  t5.playRound;System.out.println("""res5: Int = """ + $show(res$5));$skip(15); val res$6 = 
	t5.bestOf(50);System.out.println("""res6: (Int, Int) = """ + $show(res$6))}
}
