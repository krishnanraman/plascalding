/** Single Layer Perceptron Learning Algorithm: Supervised Classification
aka W = W + X

PLA Algo:
--
Given a (guess) vector W,
For all vectors X, we want sign(W dot X) == sign(X)
For any X, if sign does not agree,
Apply Update rule:
W = W + X
Start all over again with this new W:(
--

Consider a line in 2 dimensions, such as
  ax + by + c = 0
  Coefficients = (a,b,c)
  Coordinate = (x,y)
  Given the coefficients & some number n, can we come up with a coordinate about the line ?
  Sure we can!
  Let y = n
  Then (a,b,c) dot (x,n,1) = 0 => (x,n) lies on line
  So x = -(bn + c)/a = -1/a * {(b,c) dot (n,1)}
  To generalize this to larger dimensions -
  x = -1/a * {(b,c,d,e,....) dot (n,n,n,n,,,...n,1)}

  So that's the x you need to precisely lie on the line.
  If x must lie above the line, subtract off a random amount
  If x must lie below the line, add a random amount

THIS EXAMPLE:
  Consider the line
  y = 2x +  3, ie. 2x - y + 3 = 0 ie. Seq(2,-1,3)
  Start with some points above this line & the rest below.
  These points comprise the collection X

  The job of the perceptron is to find this line.
  The perceptron starts with a guess line x+y+1 = 0 ie. Seq(1,1,1) = W

  At every iteration, W is refined until it cleanly separates the collection into 2 classes.
*/
import com.twitter.scalding._
import com.twitter.scalding.ExecutionContext._
import com.twitter.algebird.monad._
import com.twitter.scalding.typed.MemorySink

class pla(args:Args)extends ExecutionContextJob(args) {

  type Weights = Seq[Double]

  def dot(a:Weights,b:Weights) = a.zip(b).map { case (c,d) => c * d } sum

  def plus(a:Weights,b:Weights) = a.zip(b).map { case (c,d) => c + d }

  def prod(a:Weights, b:Double) = a.map { _ * b }

  def scale(a:Weights) = if (math.abs(a.min) > 0.1) prod(a, 1.0/a.min) else a

  def update(w:Weights, x:(Weights,Int)) = plus(w, x match {case (weight, sign) => prod(weight, sign) }) // w = w + sign(x)*x

  def misclassified(w:Weights, x:(Weights,Int)) = x match { case (weight, sign) => math.signum(dot(w, weight)) != sign }

  def pointOnLine(coeff:Weights, n:Int):Weights = {
    val coordinates = Seq.fill[Double](coeff.size - 2)(n) ++ Seq(1.0)
    val x =  -dot(coeff.tail ,coordinates)/coeff.head
    Seq(x) ++ coordinates
  }

  def pointAboveBelowLine(coeff: Weights, n:Int, above:Boolean):Weights = {
    val pt = pointOnLine(coeff, n)
    val sign = if (above) -1 else 1
    val origX = pt.head // this x lies ON the line
    val newX = origX + sign * (2 + math.random * 7) // this x lies above(or below) the line
    Seq(newX) ++ pt.tail
  }

  override def job: Reader[ExecutionContext, Nothing] = {
      val coeff:Weights = Seq("a","b","c").map{ i=> args(i) }.map { _.toDouble } // build ax + by + c = 0
      val n = args("n").toInt // number of points

      val points = TypedPipe.from(1 to n) // generate n points
        .map {
           i =>
            val above = (i%2 == 0)
            val sign = if (above) 1 else -1
            (pointAboveBelowLine(coeff, i, above), sign)
      }

      // Only 3 mutables. FP afficionados, PLS EXCUSE
      var notConverged = true
      var iteration = 1
      var guess:Weights = Seq(1,1,1)  // x + y + 1 = 0

      while(notConverged) {

        // define some sinks
        val pointsSink = TypedTsv[(Weights, Int)]("points")
        val weightsSink = TypedTsv[Weights]("weights" + iteration)
        val sink = new MemorySink[Weights] // save the weights W here as well

        Execution.waitFor(Config.default, Local(false)) { implicit ec: ExecutionContext =>
            points
            .write(pointsSink)(flowDefFromContext, modeFromContext)
            .groupAll
            .foldLeft( guess ) {
              (currWeights, point ) =>
                if (misclassified(currWeights, point)) {
                  update(currWeights, point)
                } else currWeights
            }
            .values
            .write(sink)(flowDefFromContext, modeFromContext)
            .write(weightsSink)(flowDefFromContext, modeFromContext)
        }

        notConverged = {
          val res = sink.readResults.toList.head
          if (res != guess) {
            guess = res
            true
          } else false
        }
        iteration += 1

      }// end while

    ReaderFn(ec => { println("exiting"); sys.exit(1) } )
  }
}
