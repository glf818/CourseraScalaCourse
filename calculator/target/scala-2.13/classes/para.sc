/*
Code block after a synchronized call on an object x
is never executed by two threads at the same time.
 */


val x = new AnyRef {}
 var uidCount = 3L

def getUniqueId(): Long = x.synchronized {
  uidCount = uidCount + 1
  uidCount
}



class Account(var amount: Int = 0) {
  def transfer(that: Account, n: Int) : Unit = {
    this.synchronized {
      that.synchronized {
        that.amount -= n
        this.amount += n
      }
    }
  }
  override def toString(): String = s"Amount is $amount "
}

def tran(t1: Account, t2: Account, n: Int) ={
  val t = new Thread {
    override def run() = {
      for (i <- 0 until n )
        t1.transfer(t2, 1)
    }
  }
  t.start()
  t
}

val t1 = new Account(100000); val t2 = new Account( 200000)
val thr1 = tran(t1, t2, 30000) ; val thr2 = tran(t2, t1, 30000) ; thr1.join() ; thr2.join()

t1
t2


