package models

import anorm._
import play.api.db.DB
import play.api.Play.current

trait Model[T] { self =>

  val fields: Seq[String]
  val tableName: String
  protected var queries = Map.empty[String, SqlQuery]
  final lazy val select = s"SELECT ${fields.mkString(", ")} FROM $tableName"
  final lazy val allSql = SQL(select)
  final lazy val byIdSql = SQL(s"$select WHERE id = {id}")
  final lazy val deleteSql = SQL(s"DELETE FROM $tableName where id = {id}")
  final lazy val insertSql = SQL(
    s"""INSERT INTO $tableName(${fields.tail.mkString(", ")}
        VALUES( ${fields.tail.mkString("{", ", ", "}")})
     """
  )
  final lazy val updateSql = SQL(
    s"""UPDATE $tableName SET ${fields.tail.map(f => s"$f = {$f}")}
        WHERE id = {id}
     """
  )

  def byId(id: Long): Option[T] = DB.withConnection { implicit c =>
    byIdSql
     .on('id -> id)
     .map(bind)
     .singleOpt()
  }

  def delete(id: Long): Unit = DB.withConnection { implicit c =>
    deleteSql
      .on('id -> id)
      .executeUpdate
  }

  def all: Seq[T] = DB.withConnection{ implicit c =>
    allSql.map(bind).list()
  }

  protected def byFieldsFirstOpt(args: NamedParameter*) = DB.withConnection{ implicit c =>
    val condition = args.toSeq.map{ a => s"""${a.name} = {${a.name}}"""}.mkString(" AND ")
    queries.find(p => p._1 == condition).map(_._2).getOrElse{
      val query = SQL(s"""$select WHERE $condition""")
      queries = queries + (condition -> query)
      query
    }.on(args:_*)
     .map(bind)
     .singleOpt()
  }

  protected def byFieldsList(args: NamedParameter*) = DB.withConnection{ implicit c =>
    val condition = args.toSeq.map{ a => s"""${a.name} = {${a.name}}"""}.mkString(" AND ")
    queries.find(p => p._1 == condition).map(_._2).getOrElse{
      val query = SQL(s"""$select WHERE $condition""")
      queries = queries + (condition -> query)
      query
    }.on(args:_*)
      .map(bind)
      .list()
  }

  def bind(r: Row): T
  def update(entity: T): T
  def insert(entity: T): T
}