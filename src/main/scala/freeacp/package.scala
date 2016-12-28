import cats.~>

package object freeacp {
  type Pure[F[_]] = (() => ?) ~> F
}