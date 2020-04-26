/** /Users/rob/workspace/NeuralNet0426
 * implementing Hosmer's 1989 heart condition data, 0/1 outcomes, 100 subjects
 * to show logistic regression as the littlest neural net
 *
 * This is a  teaching example showing how a logistic model can be interpreted
 * as a simple neural net. The data is from a study by Hosmer 1989,
 * who studied 100 men from 20 to 69 with a 1 meaning a cardiac condition and
 * 0 meaning no detected condition.
 * sigma is the sigmoid (S) curve bounded by (0,1) used to represent probabilities
 * loss: the loss function = L(y, yhat) per training example, i.e for an individual pair (age, condition)
 * this is the loss for a single  training set example,
 * avgLoss == cost function =J(w,b): the average loss over all training pairs, summ the losses and average
 * ** this is the function  that is to be minimized to get the estimates, the
 * yhats == sigma(z) == 'a' to align with the 'y' values, that is, find the best w, b to do this
 * dw : is the dJ/dw   slope that determines the 'w'  direction to move for a single example
 * avgDw: is the average dw slope and determines which direction to move
 * db : is the dJ/db    per individual example
 * avgDb : is the average db slope and determines the 'b'  direction to move
 * Note 'log' is the natural log ( i.e to the base 'e')
 * Note: I represented the age as x and cardiac condition as y, to match the literature conventions
 * so that xs  is the 100 element vector of x= ages,
 * while ys  is the 100 element vector of matching y  values 0/1
 * 2020-04-26 rr
 */
import scala.math._
object NNHosmer extends App{
  type D = Double; type V = Vector[D]; type I = Integer  //type synonyms, saves typing
  def sigma(z: D) = 1/ (1 + exp(-z))
  def loss( a: D, y: D): D = -( y * log(a) + (1.0 - y)* log(1.0 - a))
  //average the individual losses to get an overall cost function J(w,b)
  def jCostFn(as: V, ys: V):D = (as zip ys).map{ case(a,y) => loss(a,y)}.sum/as.size
  // dw is actually     dJ(w,b)/dw , the effect that changing w has on the cost function
  // this is the chain rule and I want this slope to indicate how to change 'w'
  def dw(a: D, y: D, x: D):D = (a - y)* x
  def avgDw ( as:V, ys: V, xs: V):D =
    ((as zip ys) zip xs).map{case( (a,y),x) => dw(a,y,x)}.sum/as.size
  // this is the individual slope of dJ/db
  def db( a:D, y:D):D = a - y
  // this is the average slope over all the training examples
  def avgDb(as: V, ys: V):D = (as zip ys).map{ case(a,y) => a-y}.sum/as.size
  //the recursive function to calc new w and b with no mutation,  no variable are modified....**!
  // How many iterations do you want, and how often do you want to see a print out?
  // xs iw a vector of ages, while ys is a vector of conditions, 0/1
  // alpha, the learning rate is set by you. Small is better so no over-shoot
  def findBest_W_B(w: D, b: D, iterations:I, printInterval: I, maxiterations:I):Unit= {
    val zs  = xs.map{w * _ + b}
    val as  = zs.map(sigma)
    val jcostfn = jCostFn(as,ys)
    val avgdw   = avgDw(as, ys, xs)
    val avgdb   = avgDb(as,ys)
    val output = f" w $w%2.3f b $b%2.3f J(w,b) $jcostfn%2.3f  dJ/dw $avgdw%2.3f dJ/db $avgdb%2.3f iterations $iterations"
    if(iterations%printInterval==0) println(output) else ""
    if (iterations >= maxiterations) {
      println(output)
      hosmerRaw.close()  // close the open file
    }

    // note, w, b are NOT modified ( mutated) instead, new values are created
    else
    findBest_W_B(w - alpha*avgdw, b - alpha*avgdb , iterations+1, printInterval, maxiterations)
  }//end findBest_W_B

  // raw local data set read in: ( a little cleaning of  empty lines was necessary)
  val fn = "/Users/rob/Desktop/HosmerCHD.txt"

  //val hosmerRaw = io.Source.fromFile(fn).getLines().toVector
  //   .filter(_ != "")
  val hosmerRaw = io.Source.fromFile(fn)
  val hosmer = hosmerRaw.getLines().toVector.filter(_ != "")
  println(hosmer.size)
  val pairs = hosmer.map{ line => {
    val Array(nr,age,chd) =line.split(" ")
    (age.toDouble, chd.toDouble)
  }}
  println(pairs.take(5))
  val (xs,ys) = pairs.unzip
  println(xs.take(5))

  val alpha = 0.0048
  //findBest_W_B(w: D, b: D, iterations:I, printInterval: I, maxiterations: I)
  findBest_W_B( 0.0, 0.0, 0, 500 ,  60000 )


}// end object NNhosmer
