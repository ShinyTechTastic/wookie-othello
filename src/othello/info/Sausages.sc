package othello.info
import scala.util.Random

object Sausages {
	
	val mmplayer = new MiniMax(2 , MiniMax.tileCount )
                                                  //> mmplayer  : othello.info.MiniMax = othello.info.MiniMax@79f6f296
	mmplayer.choose( Board.initial.validMoves , Board.initial )
                                                  //> res0: (Int, Int) = (3,5)
                   
	MiniMax.tileCount(Black)((3,5), Board.initial)
                                                  //> res1: Double = 3.0
	MiniMax.tileCount(White)((3,5), Board.initial)
                                                  //> res2: Double = -3.0

//	val t4 = new Tournament( new PrintPlayer(mmplayer,"mm") , new PrintPlayer(RandomPlayer,"random") )
	val t4 = new Tournament( mmplayer , RandomPlayer )
                                                  //> t4  : othello.info.Tournament = othello.info.Tournament@39e87719
                    

  t4.playRound                                    //> res3: Int = 2
  //t4.bestOf(50)
                           
                           
  val rp = GeneticPlayer.randomPlayer( new Random() )
                                                  //> rp  : othello.info.GeneticPlayer = <function1>
                      
 	val rpplayer = new MiniMax(2 , rp )       //> rpplayer  : othello.info.MiniMax = othello.info.MiniMax@c75e4fc
	rpplayer.choose( Board.initial.validMoves , Board.initial )
                                                  //> res4: (Int, Int) = (5,3)
                                     
 	val t5 = new Tournament( rpplayer , RandomPlayer )
                                                  //> t5  : othello.info.Tournament = othello.info.Tournament@781f6226
 
  t5.playRound                                    //> res5: Int = 2
	t5.bestOf(50)                             //> res6: (Int, Int) = (50,40)
}