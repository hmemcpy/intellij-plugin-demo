package inspections

import org.jetbrains.plugins.scala.codeInspection.collections.{OperationOnCollectionInspection, SimplificationType, _}
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression

class SimplifyMapInspection extends OperationOnCollectionInspection {
  setLikeCollectionClasses(Array("zio._"))

  override def possibleSimplificationTypes: Array[SimplificationType] =
    Array(AsSimplification, ExitCodeSimplification)
}

object AsSimplification extends SimplificationType {
  override def hint: String = "Replace with .as"

  override def getSimplification(expr: ScExpression): Option[Simplification] =
    // qualifier.map(_ => x) -> x
    expr match {
      case qual `.map` `_ => x`(x) => Some(replacement(expr, qual, x))
      case _                       => None
    }

  private def replacement(expr: ScExpression, qual: ScExpression, body: ScExpression) =
    replace(expr).withText(invocationText(qual, s"as(${body.getText}"))
}

object ExitCodeSimplification extends SimplificationType {
  override def hint: String = "Replace with .exitCode"

  override def getSimplification(expr: ScExpression): Option[Simplification] =
    // qualifier.map(_ => x) -> x
    expr match {
      case qual `.map` `_ => x`(x)
          if x.textMatches("ExitCode.success") ||
            x.textMatches("ExitCode.failure") =>
        Some(replacement(expr, qual))
      case _ => None
    }

  private def replacement(expr: ScExpression, qual: ScExpression) =
    replace(expr).withText(invocationText(qual, s"exitCode"))
}
