package io.circe

import cats.{ ApplicativeError, MonadError }
import io.circe.jawn.CirceSupportParser
import io.iteratee.{ Enumeratee, Enumerator }
import org.typelevel.jawn.{ AsyncParser, ParseException }

package object iteratee {
  private[this] val supportParser: CirceSupportParser = new CirceSupportParser(None, true)

  /**
   * String parser of JSON values wrapped in a single array
   */
  final def stringArrayParser[F[_]](implicit F: ApplicativeError[F, Throwable]): Enumeratee[F, String, Json] =
    stringParser(AsyncParser.UnwrapArray)

  /**
   * String parser of JSON values delimited by whitespace/newline
   */
  final def stringStreamParser[F[_]](implicit F: ApplicativeError[F, Throwable]): Enumeratee[F, String, Json] =
    stringParser(AsyncParser.ValueStream)

  /**
   * Byte array parser of JSON values wrapped in a single array
   */
  final def byteArrayParser[F[_]](implicit F: ApplicativeError[F, Throwable]): Enumeratee[F, Array[Byte], Json] =
    byteParser(AsyncParser.UnwrapArray)

  /**
   * Byte array parser of JSON values delimited by whitespace/newline
   */
  final def byteStreamParser[F[_]](implicit F: ApplicativeError[F, Throwable]): Enumeratee[F, Array[Byte], Json] =
    byteParser(AsyncParser.ValueStream)

  /**
   * Enumeratee decoder from JSON to some type `A`
   */
  final def decoder[F[_], A](implicit F: MonadError[F, Throwable], decode: Decoder[A]): Enumeratee[F, Json, A] =
    Enumeratee.flatMap(json =>
      decode(json.hcursor) match {
        case Left(df) => Enumerator.liftM(F.raiseError(df))
        case Right(a) => Enumerator.enumOne(a)
      }
    )

  private def stringParser[F[_]](
    mode: AsyncParser.Mode
  )(implicit F: ApplicativeError[F, Throwable]): Enumeratee[F, String, Json] =
    new ParsingEnumeratee[F, String](supportParser) {
      protected[this] final def parseWith(p: AsyncParser[Json])(in: String): Either[ParseException, Seq[Json]] =
        p.absorb(in)(supportParser.facade).map(_.toSeq)

      protected[this] val parsingMode: AsyncParser.Mode = mode
    }

  private def byteParser[F[_]](
    mode: AsyncParser.Mode
  )(implicit F: ApplicativeError[F, Throwable]): Enumeratee[F, Array[Byte], Json] =
    new ParsingEnumeratee[F, Array[Byte]](supportParser) {
      protected[this] final def parseWith(p: AsyncParser[Json])(in: Array[Byte]): Either[ParseException, Seq[Json]] =
        p.absorb(in)(supportParser.facade).map(_.toSeq)

      protected[this] val parsingMode: AsyncParser.Mode = mode
    }
}
