package othello.info

object Sausages {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(93); 
	
	val mmplayer = new MiniMax(2 , MiniMax.tileCount );System.out.println("""mmplayer  : othello.info.MiniMax = """ + $show(mmplayer ));$skip(61); val res$0 = 
	mmplayer.choose( Board.initial.validMoves , Board.initial );System.out.println("""res0: (Int, Int) = """ + $show(res$0));$skip(42); val res$1 = 

	MiniMax.tileCount((3,5), Board.initial);System.out.println("""res1: Double = """ + $show(res$1));$skip(169); 
               

//	val t4 = new Tournament( new PrintPlayer(mmplayer,"mm") , new PrintPlayer(RandomPlayer,"random") )
	val t4 = new Tournament( mmplayer,RandomPlayer );System.out.println("""t4  : othello.info.Tournament = """ + $show(t4 ));$skip(16); val res$2 = 

  t4.playRound;System.out.println("""res2: Int = """ + $show(res$2))}
                           
	
}
