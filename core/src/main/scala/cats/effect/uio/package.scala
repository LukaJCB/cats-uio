package cats.effect

package object uio {
  type UIO[A] = UIO.Type[A]
}
