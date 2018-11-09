package lavender.util

import cats.implicits._
import cats.{Applicative, Monad}
import cats.data.StateT
import monocle.{Getter, Lens, Setter}

import scala.language.{higherKinds, implicitConversions}

object monocles {
  implicit def getterOps[S, A](getter: Getter[S, A]): GetterOps[S, A] = new GetterOps(getter)
  implicit def lensGetterOps[S, A](lens: Lens[S, A]): GetterOps[S, A] = new GetterOps(lens.asGetter)
  implicit def setterOps[S, A](setter: Setter[S, A]): SetterOps[S, A] = new SetterOps(setter)
  implicit def lensSetterOps[S, A](lens: Lens[S, A]): SetterOps[S, A] = new SetterOps(lens.asSetter)
  implicit def lensOps[S, A](lens: Lens[S, A]): LensOps[S, A] = new LensOps(lens)
  final class GetterOps[S, A](val getter: Getter[S, A]) extends AnyVal {
    def extract[F[_] : Applicative]: StateT[F, S, A] = StateT.get[F, S].map(getter.get)
  }
  final class SetterOps[S, A](val setter: Setter[S, A]) extends AnyVal {
    def assign[F[_] : Applicative](a: A): StateT[F, S, Unit] =
      StateT.modify[F, S](setter set a)
    def modifying[F[_] : Applicative](f: A => A): StateT[F, S, Unit] =
      StateT.modify[F, S](setter modify f)

    def :=[F[_] : Applicative](a: A): StateT[F, S, Unit] = assign[F](a)
    def %=[F[_] : Applicative](f: A => A): StateT[F, S, Unit] = modifying[F](f)
  }
  final class LensOps[S, A](val lens: Lens[S, A]) extends AnyVal {
    def transform[F[_] : Monad, B](st: StateT[F, A, B]): StateT[F, S, B] = StateT.apply(s => st.run(lens.get(s)).map {
      case (s1, a) => (lens.set(s1)(s), a)
    })
    def %%=[F[_] : Monad, B](st: StateT[F, A, B]): StateT[F, S, B] = transform(st)
  }
}
