package forcomp



trait Generator[+T] {
  import java.util.Random
  val rand = new Random
  self =>
  def generate: T
  def map[U](f: T=>U): Generator[U] =
    new Generator[U] {
      def generate = f(self.generate)
    }
  def flatMap[S](f : T=>Generator[S]): Generator[S] = new Generator[S]{
    def generate: S = f(self.generate).generate
  }
}
