package mnru.matrix


object MatrixField {

  type F[T] = (T, T) => T
  type R[T] = List[T]
  type M[T] = List[R[T]]


  import scala.math.Numeric


  class Matrix[T: Numeric](val elms: M[T]) {


    override def toString() = (this.elms map (_.mkString("[", ",", "]"))).mkString("", "\n", "\n")

   def rop(rthis:R[T],rthat:R[T])(op: F[T]):R[T]=(rthis zip rthat)map (t => op(t._1,t._2)) 
    
    def mop(that: Matrix[T])(op: F[T]): Matrix[T] = new Matrix[T](
       ( this.elms zip that.elms) map (t => rop(t._1,t._2)(op(_,_)))
      )

    ///	 def mop(that: Matrix[T])(op: F[T]): Matrix[T] = new Matrix[T](((this.elms zip that.elms).map(t => ((t._1 zip t._2)).map(s => op(s._1, s._2)))))

    def +(a: T, b: T) = implicitly[Numeric[T]].plus(a, b)

    def -(a: T, b: T) = implicitly[Numeric[T]].minus(a, b)

    def *(a: T, b: T) = implicitly[Numeric[T]].times(a, b)


    def +(that: Matrix[T]): Matrix[T] = this.mop(that)(this.+(_, _))

    def -(that: Matrix[T]): Matrix[T] = this.mop(that)(this.-(_, _))


    def transpose = new Matrix[T](this.elms.transpose)


    def dotProduct(a: R[T], b: R[T]): T = (for ((x, y) <- a zip b) yield this.*(x, y)).reduceLeft(this.+(_, _))

    def *(that: Matrix[T]): Matrix[T] = new Matrix[T](
      (for (rthis <- this.elms) yield
        (for (cthat <- that.elms.transpose) yield
          dotProduct(rthis, cthat)))
    )

  }


  val matA = new Matrix[Int](List(1 :: 2 :: 3 :: Nil, 4 :: 5 :: 6 :: Nil))
  val matB = new Matrix[Int](List(7 :: 8 :: 9 :: Nil, 10 :: 11 :: 12 :: Nil))
  //  val matC = matA.mop(matB)(_ + _)
  val matC = matA.transpose
  val matD = matA + matB
  val matE = matB * matC

  def main(args: Array[String]) {


    println(matA)
    println(matB)
    println(matC)
    println(matD)
    println(matE)


  }


}



