import cats.~>

package object freeacp {
  type Suspended[F[_]] = (() => ?) ~> F
}