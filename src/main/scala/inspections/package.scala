import org.jetbrains.plugins.scala.codeInspection.collections.{
  MethodRepr,
  Qualified,
  invocation,
  isOfClassFrom,
  stripped
}
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.ScReferencePattern
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScExpression, ScFunctionExpr, ScMethodCall, ScReferenceExpression}
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunctionDefinition
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScParameter

package object inspections {

  val zioPackage = Array("zio._")

  val `.map`: Qualified = invocation("map").from(zioPackage)

  def fromZio(expr: ScExpression): Boolean =
    isOfClassFrom(expr, zioPackage)

  /**
   * Matches a lambda expression (e.g. a => b)
   */
  object lambda {

    def unapply(expr: ScExpression): Option[(Seq[ScParameter], Option[ScExpression])] = expr match {
      case ScFunctionExpr(params @ Seq(_), res @ Some(_)) =>
        Some(params, res.map(stripped))
      case _ => None
    }
  }

  /**
   * Matches a lambda expression with the parameter discarded (e.g. _ => b)
   */
  object `_ => x` {

    def unapply(expr: ScExpression): Option[ScExpression] = expr match {
      case lambda(Seq(x), body) if x.name == "_" => body
      case _                                     => None
    }
  }

  /**
   * Checks whether a given expression belongs to ZIO
   */
  object zioRef {

    def unapply(expr: ScExpression): Option[(ScReferenceExpression, ScExpression)] = expr match {
      case ref @ ScReferenceExpression(_) =>
        ref.resolve() match {
          case _: ScReferencePattern | _: ScFunctionDefinition if fromZio(expr) => Some((ref, expr))
          case _                                                                => None
        }
      case MethodRepr(_, _, Some(ref), Seq(e)) =>
        ref.resolve() match {
          case _ if fromZio(expr) => Some((ref, e))
          case _                  => None
        }
      // multiple argument lists
      case ScMethodCall(ScMethodCall(ref @ ScReferenceExpression(_), Seq(_)), Seq(_)) if fromZio(expr) =>
        Some((ref, expr))
      case _ => None
    }
  }
}
