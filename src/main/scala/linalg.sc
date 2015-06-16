import breeze.linalg._
import breeze.optimize._
import breeze.numerics._
import com.devdiscoveries.ml.Predef._
import com.devdiscoveries.ml.functions.Functions

object linalg {
	println("test")                           //> test

  val f = new DiffFunction[DenseVector[Double]] {
  	def calculate(x: DenseVector[Double]) = {
  		(norm((x - 3.) :^ 2.,1.),(x * 2.) - 6.)
  	}
  }                                               //> f  : breeze.optimize.DiffFunction[breeze.linalg.DenseVector[Double]] = <func
                                                  //| tion1>
  f.valueAt(DenseVector(3,3,3))                   //> java.lang.NoSuchMethodError: scala.Predef$.ArrowAssoc(Ljava/lang/Object;)Lja
                                                  //| va/lang/Object;
                                                  //| 	at breeze.generic.MMRegistry2$class.register(Multimethod.scala:188)
                                                  //| 	at breeze.linalg.VectorOps$$anon$1.breeze$linalg$operators$BinaryRegistr
                                                  //| y$$super$register(Vector.scala:303)
                                                  //| 	at breeze.linalg.operators.BinaryRegistry$class.register(BinaryOp.scala:
                                                  //| 87)
                                                  //| 	at breeze.linalg.VectorOps$$anon$1.register(Vector.scala:303)
                                                  //| 	at breeze.linalg.operators.DenseVectorOps$$anon$1.<init>(DenseVectorOps.
                                                  //| scala:38)
                                                  //| 	at breeze.linalg.operators.DenseVectorOps$class.$init$(DenseVectorOps.sc
                                                  //| ala:22)
                                                  //| 	at breeze.linalg.DenseVector$.<init>(DenseVector.scala:225)
                                                  //| 	at breeze.linalg.DenseVector$.<clinit>(DenseVector.scala)
                                                  //| 	at linalg$$anonfun$main$1.apply$mcV$sp(linalg.scala:15)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.libra
                                                  //| Output exceeds cutoff limit.
}