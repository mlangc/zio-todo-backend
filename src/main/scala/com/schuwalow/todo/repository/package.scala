package com.schuwalow.todo

import com.github.mlangc.slf4zio.api._
import zio._
import zio.clock.Clock

package object repository {
  type TodoRepository            = Has[TodoRepository.Service]
  type TodoRepositoryWithLogging = TodoRepository with Logging with Clock

  private def mkLogSpec(method: String): LogSpec[Any, Any] =
    LogSpec.onSucceed(d => debug"${d.render} for $method")

  private def withRepo[R, E, A](
    f: TodoRepository.Service => ZIO[R, E, A]
  ): ZIO[R with TodoRepository, E, A] =
    ZIO.accessM[R with TodoRepository](repo => f(repo.get))

  def create(
    todoItemForm: TodoItemPostForm
  ): ZIO[TodoRepositoryWithLogging, Nothing, TodoItem] =
    withRepo(_.create(todoItemForm))
      .perfLogZ(mkLogSpec(s"create($todoItemForm)"))

  def getById(
    id: TodoId
  ): ZIO[TodoRepositoryWithLogging, Nothing, Option[TodoItem]] =
    withRepo(_.getById(id))
      .perfLogZ(mkLogSpec(s"getById($id)"))

  def getAll: ZIO[TodoRepositoryWithLogging, Nothing, List[TodoItem]] =
    withRepo(_.getAll)
      .perfLogZ(mkLogSpec("getAll"))

  def delete(id: TodoId): ZIO[TodoRepositoryWithLogging, Nothing, Unit] =
    withRepo(_.delete(id))
      .perfLogZ(mkLogSpec(s"delete($id)"))

  def deleteAll: ZIO[TodoRepositoryWithLogging, Nothing, Unit] =
    withRepo(_.deleteAll)
      .perfLogZ(mkLogSpec(s"deleteAll"))

  def update(
    id: TodoId,
    todoItemForm: TodoItemPatchForm
  ): ZIO[TodoRepositoryWithLogging, Nothing, Option[TodoItem]] =
    withRepo(_.update(id, todoItemForm))
      .perfLogZ(mkLogSpec(s"update($id, $todoItemForm)"))

}
