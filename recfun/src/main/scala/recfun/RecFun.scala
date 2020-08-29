package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =  if (c > r) 0 else ( if (c==0) 1 else pascal(c-1, r-1)+pascal(c, r-1))


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def checkBalance(lft:String, rgt:List[Char]):Boolean = {
      if (( (lft!="" && lft.head== ')')) || (rgt.isEmpty && lft!="")) false else(
        if (rgt.isEmpty && lft=="") true else
          checkBalance(if (lft=="" || lft.last == rgt.head) (lft + rgt.head) else  lft.init, rgt.tail )
        )
    }
    val pnList = chars.filter(chr=> (chr=='(') || (chr==')') )
    checkBalance("", pnList)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    val coins_ = coins.sorted
    val coinMin = coins_.min
    val coinMax = coins_.max
    def ch(coinIdx: Int, faceV: Int): Int=
    {
      if(faceV==0) 1
      else if (faceV < 0) 0
      else if(coinIdx==0 && faceV % coins_(coinIdx) ==0) 1
      else if (coinIdx==0 && faceV % coins_(coinIdx) >0) 0
      else ch(coinIdx-1, faceV) + ch(coinIdx, faceV-coins_(coinIdx))
    }
    ch(coins_.size-1, money)
  }
}
