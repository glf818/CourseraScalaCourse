import org.scalacheck.Prop
import org.scalacheck.Properties


import org.scalacheck.Arbitrary._
import org.scalacheck.Prop
import org.scalacheck.Prop._
import org.scalacheck.Test.{check, Result, Failed, PropException}
import quickcheck._



object QuickCheckBinomialHeap extends QuickCheckHeap with BinomialHeap

/** Turns a `Properties` instance into a single `Prop` by combining all the properties */
def asProp(properties: Properties): Prop = Prop.all(properties.properties.map(_._2).toSeq:_*)

QuickCheckBinomialHeap.properties.map(_._2).toSeq:_*



