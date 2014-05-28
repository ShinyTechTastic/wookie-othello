package othello.info

object Sausages {
	
	val mmplayer = new MiniMax(2 , MiniMax.tileCount )
                                                  //> mmplayer  : othello.info.MiniMax = othello.info.MiniMax@4de13d52
	mmplayer.choose( Board.initial.validMoves , Board.initial )
                                                  //> res0: (Int, Int) = (3,5)

	MiniMax.tileCount((3,5), Board.initial)   //> res1: Double = 3.0
               

//	val t4 = new Tournament( new PrintPlayer(mmplayer,"mm") , new PrintPlayer(RandomPlayer,"random") )
	val t4 = new Tournament( mmplayer,RandomPlayer )
                                                  //> t4  : othello.info.Tournament = othello.info.Tournament@6ef137d

  t4.playRound                                    //> res2: Int = 2
                           
	
}