import java.util.concurrent._
import kmeans._
import scala.collection.{Map, Seq, mutable}
import scala.collection.parallel.{ParMap, ParSeq}
import scala.collection.parallel.CollectionConverters._
import scala.math._

val km = new kmeans.KMeans()
val ptSeqs = km.generatePoints(2, 10);
val means = km.initializeMeans(3,  ptSeqs)
//val ptSeqsClosest = ptSeqs.map(x => km.findClosest(x, means))
val grpPt1 = ptSeqs.groupBy(x => km.findClosest(x, means))

val extraPt =  new Point(1.3, -1, -0.2)
grpPt1.contains(extraPt)
val emptSeq = Seq[Point]()
grpPt1 + (extraPt -> Seq[Point]().to(mutable.ArrayBuffer))







//.generatePoints(2, 10);
