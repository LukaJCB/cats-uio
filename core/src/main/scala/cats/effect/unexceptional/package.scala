package cats.effect

package object unexceptional {
  type UIO[A] = UIO.Type[A]
  type Unexceptional[F[_], A] = Unexceptional.Type[F, A]
}
