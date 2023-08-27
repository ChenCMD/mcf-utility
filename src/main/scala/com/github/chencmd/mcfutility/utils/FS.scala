package com.github.chencmd.mcfutility.utils

import com.github.chencmd.mcfutility.generic.EitherTExtra

import cats.effect.Async
import cats.effect.implicits.*
import cats.effect.kernel.Resource
import cats.implicits.*
import cats.mtl.Raise

import java.io.File
import java.io.FileWriter
import java.nio.charset.Charset
import mouse.all.*

object FS {
  private def getFileWriter[F[_]](path: String)(using F: Async[F]): Resource[F, FileWriter] = {
    // Resource.fromAutoCloseable(F.delay(FileWriter(File(path), Charset.forName("UTF8"))))
    ???
  }

  def pathAccessible[F[_]](path: String)(using F: Async[F]): F[Boolean] = {
    F.delay(File(path).exists())
  }

  def writeFile[F[_]](path: String, contents: List[String])(using F: Async[F]): F[Unit] = {
    getFileWriter(path).use(fw => F.delay(fw.write(contents.mkString("\n")))).void
  }

  def readDir[F[_]](path: String, recursive: Boolean = false)(using
    F: Async[F],
    R: Raise[F, String]
  ): F[List[String]] = {
    def rec(file: File): F[List[String]] = {
      val program = for {
        _          <- F.delay(file.exists().either(List.empty, ())).liftEitherT
        _          <- F.delay(file.isDirectory().either(List.empty, ())).liftEitherT
        _          <- F
          .delay(file.canRead())
          .ifM(
            F.unit,
            R.raise("Error opening directory '$d': Permission denied")
          )
          .liftEitherT
        children   <- F.delay(file.listFiles().toList).liftEitherT
        files      <- children.traverseFilter(c => F.delay(c.isFile().option(c))).liftEitherT
        _          <- EitherTExtra.exitWhenA(!recursive)(files.map(_.getPath).pure[F])
        dirs       <- children.traverseFilter(c => F.delay(c.isDirectory().option(c))).liftEitherT
        childFiles <- dirs.flatTraverse(rec).liftEitherT
      } yield files.map(_.getPath()) ::: childFiles

      program.value.map(_.merge)
    }

    rec(File(path))
  }
}
