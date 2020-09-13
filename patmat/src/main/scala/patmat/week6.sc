object nQueen
{
  val n = 8
  def placeQueen(row: Int): Set[List[Int]] = {
    if (row == 0) Set(List[Int]())
    else for {
      oldSols <- placeQueen(row - 1)
      newCol <- 0 until n
      if isSafe(newCol, oldSols)
    } yield newCol :: oldSols
  }

  def isSafe(newCol: Int,  oldSols: List[Int]): Boolean = {
    val rowN = oldSols.length
    val rows = rowN-1 to 0 by -1
    val pairs = rows zip oldSols
    pairs forall {case (r, c)=> (c!=newCol) && (math.abs(c-newCol)!= (rowN-r))  }
  }

  /*def showOneSol(sols: List[Int]):String = {
    val solsStr = sols.reverse  map (col=> Vector.fill(sols.length)("*    ").updated(col, "nmsl ").mkString(","))
    "\n"+solsStr.mkString("\n")
  }*/
}

/*
nQueen.placeQueen(8) take(3) map nQueen.showOneSol foreach println

val x = Map("apple"->0,"abanana"->1, "cantelope"->2, "cdorian"->3)
x groupBy  {case (k,v)=>k.head}
 */

val mnem = Map('2'->"ABC", '3'->"DEF") + ('4'->"GHI") + ('5'->"JKL")
val revd =  for {(keyChar, ss ) <- mnem} yield {
  ss.map((_,keyChar))
}
val charCode = revd.flatten.toMap
mnem.foldLeft(Map[Char,Char]()) {(acc, p) =>
  acc ++ p._2.map((_, p._1)).toMap }

val aWord = "nmsl"
aWord forall (_.isLetter)












































