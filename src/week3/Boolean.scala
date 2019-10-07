package idealized.sacla

object Boolean {

  abstract class Boolean {

    def ifThenElse[T](t: => T, e: => T): T

    def && (x: => Boolean ): Boolean = ifThenElse(x, false)

    def || (x: => Boolean): Boolean = ifThenElse[Boolean](true, x)

    def unary_! : Boolean = ifThenElse[Boolean](false, true)

    def == (x: Boolean): Boolean = ifThenElse[Boolean](x, x.unary_!)
    def != (x: Boolean): Boolean = ifThenElse[Boolean](x.unary_!, x)

  }

  object true extends Boolean {
    def ifThenElse[T](t: =>T, e: =>T)= t
  }

  object false extends Boolean {
    def ifThenElse[T](t: =>T, e: =>T)= e

  }

}




