package com.schuwalow.todo.http

import com.github.ghik.silencer.silent
import com.github.ghik.silencer.silent
import com.github.mlangc.slf4zio.api._
import io.circe.generic.semiauto._
import io.circe.{ Decoder, Encoder }
import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.util.CaseInsensitiveString
import zio._
import zio.interop.catz._

import com.schuwalow.todo._
import com.schuwalow.todo.repository._
import com.schuwalow.todo.{
  TodoId,
  TodoItem,
  TodoItemPatchForm,
  TodoItemPostForm
}

object TodoService {

  final case class TodoItemWithUri(
    id: Long,
    url: String,
    title: String,
    completed: Boolean,
    order: Option[Int])

  object TodoItemWithUri {

    def apply(
      basePath: String,
      todoItem: TodoItem
    ): TodoItemWithUri =
      TodoItemWithUri(
        todoItem.id.value,
        s"$basePath/${todoItem.id.value}",
        todoItem.item.title,
        todoItem.item.completed,
        todoItem.item.order
      )

    implicit val encoder: Encoder[TodoItemWithUri] = deriveEncoder
    implicit val decoder: Decoder[TodoItemWithUri] = deriveDecoder
  }

  @silent("unreachable") // https://github.com/scala/bug/issues/11457
  def routes[R <: TodoRepositoryWithLogging](
    rootUri: String
  ): HttpRoutes[RIO[R, ?]] = {
    type TodoTask[A] = RIO[R, A]

    def handle[A](request: Request[TodoTask])(op: TodoTask[A]): TodoTask[A] = {
      val correlationId =
        request.headers
          .get(CaseInsensitiveString("X-Correlation-ID"))
          .map(h => "corrId" -> h.value)

      logging.mdzio.doWith(correlationId)(op)
    }

    val dsl: Http4sDsl[TodoTask] = Http4sDsl[TodoTask]
    import dsl._

    implicit def circeJsonDecoder[A](
      implicit
      decoder: Decoder[A]
    ): EntityDecoder[TodoTask, A] =
      jsonOf[TodoTask, A]
    implicit def circeJsonEncoder[A](
      implicit
      encoder: Encoder[A]
    ): EntityEncoder[TodoTask, A] =
      jsonEncoderOf[TodoTask, A]

    HttpRoutes.of[TodoTask] {

      case req @ GET -> Root / LongVar(id) =>
        handle(req) {
          for {
            _    <- logging.debugIO(s"Headers: ${req.headers}")
            _    <- logging.debugIO(s"Loading item with id $id")
            todo <- getById(TodoId(id))
            response <- todo.fold(NotFound())(
                         x => Ok(TodoItemWithUri(rootUri, x))
                       )
          } yield response
        }

      case req @ GET -> Root =>
        handle(req) {
          logging.debugIO("Loading all items") *>
            Ok(getAll.map(_.map(TodoItemWithUri(rootUri, _))))
        }

      case req @ POST -> Root =>
        handle(req) {
          req.decode[TodoItemPostForm] { todoItemForm =>
            logging.debugIO(s"Creating todo item $todoItemForm") *>
              create(todoItemForm)
                .map(TodoItemWithUri(rootUri, _))
                .flatMap(Created(_))
          }
        }

      case req @ DELETE -> Root / LongVar(id) =>
        handle(req) {
          for {
            _    <- logging.debugIO(s"Deleting todo item $id")
            item <- getById(TodoId(id))
            result <- item
                       .map(x => delete(x.id))
                       .fold(NotFound())(_.flatMap(Ok(_)))
          } yield result
        }

      case req @ DELETE -> Root =>
        handle(req) {
          logging.debugIO("Deleting all todo items") *> deleteAll *> Ok()
        }

      case req @ PATCH -> Root / LongVar(id) =>
        handle(req) {
          req.decode[TodoItemPatchForm] { updateForm =>
            for {
              _      <- logging.debugIO(s"Updating todo item $id with $updateForm")
              update <- update(TodoId(id), updateForm)
              response <- update.fold(NotFound())(
                           x => Ok(TodoItemWithUri(rootUri, x))
                         )
            } yield response
          }
        }
    }
  }
}
