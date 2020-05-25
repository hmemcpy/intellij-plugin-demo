package inspections

import org.jetbrains.plugins.scala.codeInspection.collections.{OperationOnCollectionInspection, SimplificationType, _}
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression

class SimplifyMapInspection extends OperationOnCollectionInspection {
  setLikeCollectionClasses(Array("zio._"))

  override def possibleSimplificationTypes: Array[SimplificationType] =
    Array(AsSimplification)
}

object AsSimplification extends SimplificationType {
  override def hint: String = "Replace with .as"

  override def getSimplification(expr: ScExpression): Option[Simplification] =
    ???

  private def replacement(expr: ScExpression, qual: ScExpression, body: ScExpression) =
    replace(expr).withText(invocationText(qual, s"as(${body.getText}"))
}