import codecs.Util.{parseJson, renderJson}
import codecs._
import org.typelevel.jawn.{Parser, SimpleFacade}

implicit val facade: SimpleFacade[Json] = new SimpleFacade[Json] {
  def jnull() = Json.Null
  def jtrue() = Json.Bool(true)
  def jfalse() = Json.Bool(false)
  def jnum(s: CharSequence, decIndex: Int, expIndex: Int) =
    Json.Num(BigDecimal(s.toString))
  def jstring(s: CharSequence) = Json.Str(s.toString)
  def jarray(vs: List[Json]) = Json.Arr(vs)
  def jobject(vs: Map[String, Json]) = Json.Obj(vs)
}

import org.scalacheck
import org.scalacheck.{ Gen, Prop }
import org.scalacheck.Prop.propBoolean

val p = Gen.resultOf((s: String, x: Int) => Person(s, x))
val peopleGenerator = Gen.listOf(p)
for(i <- 1 to 10) yield p.sample.get

val pEn = implicitly[Encoder[Person]]
val pDe = implicitly[Decoder[Person]]
val aP = Person("Dim", 35)
val apEn = pEn.encode(aP)
val maybeDecoded = pDe.decode(apEn)
val x =  maybeDecoded.contains(aP)

val prop = maybeDecoded.contains(aP) :| "nmslre"


















