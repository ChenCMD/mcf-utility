package com.github.chencmd.mcfutility.generic

import cats.Applicative
import cats.data.EitherT

object EitherTExtra {
  def exitWhenA[F[_]: Applicative, A](cond: Boolean)(exitValue: => F[A]): EitherT[F, A, Unit] = {
    if cond then EitherT.left(exitValue)
    else EitherT.pure(())
  }
}
