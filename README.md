# cats-uio

This library provides an IO type that is guaranteed to have no exceptions.
This can be useful, as usually when you're given an `IO[A]`, you never quite know if the computation will be successful or not.
Furthermore, even when you use `attempt`, you'll be left with an `Either[Throwable, A]`, which gives you almost no information on which errors might actually occur.
`UIO` mitigates these problems by requiring you to be explicit when a computation might fail.

Another problem that might occur with `IO` is that when you're using an `EitherT` monad transformer over `IO`, you end up with two different instances of `MonadError` for the same type.
I.e. you have both `MonadError[EitherT[IO, E, ?], E]` and `MonadError[EitherT[IO, E, ?], Throwable]`, which can lead to some subtle bugs.
With `EitherT[UIO, E, ?], E]` there's only one error type and therefore only one `MonadError` instance.

## Features

`UIO` comes with almost all features the current `cats.effect.IO` has, which includes running two `UIO`s in parallel,
racing them, cancelling a running process and more!


### Caveats

Because of the way the `cats-effect` type classes are structured, unfortunately `UIO` cannot work with them directly, as they all require `MonadError`, which `UIO` can't provide.
