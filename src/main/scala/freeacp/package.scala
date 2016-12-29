import cats.~>

package object freeacp {
  type Suspended[F[_]] = (() => ?) ~> F
  def Suspended[F[_]](implicit e: Suspended[F]) = e
}