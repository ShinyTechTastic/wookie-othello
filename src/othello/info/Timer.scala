package othello.info
import java.util.Calendar

object Timer {
  def getTime() = Calendar.getInstance().getTime().getTime()
  
  
  def timeFunction[A,B]( functionName:String , function:Function1[A,B] ):(A=>B) = {
	  def inner(p1:A):B = {
		  Timer.log( functionName +" start")
		  val start = this.getTime();
		  val rv = function.apply( p1 )
		  val end = this.getTime();
		  Timer.log( functionName +" stop ("+(end-start)+")")
		  rv
	  }
	  inner _
  }
  
  def timeFunction[A,B,C]( functionName:String , function:Function2[A,B,C] ):((A,B)=>C) = {
	  def inner(p1:A,p2:B):C = {
		  Timer.log( functionName +" start")
		  val start = this.getTime();
		  val rv = function.apply( p1 , p2 )
		  val end = this.getTime();
		  Timer.log( functionName +" stop ("+(end-start)+")")
		  rv
	  }
	  inner _
  }
	  
	 def log( s:String ){
	   println( s );
	 }
}