package cats.effect

package object unexceptional {
  type UIO[A] = UIOImpl.Type[A]
  val UIO = UIOImpl
  type Unexceptional[F[_], A] = UnexceptionalImpl.Type[F, A]
  val Unexceptional = UnexceptionalImpl
}
