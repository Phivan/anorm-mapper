package models

import anorm._
import play.api.db.DB
import play.api.Play.current

trait Model[T] { self =>

  val fields: Seq[String]
  val tableName: String
  val parser: RowParser[T]
  protected var queries = Map.empty[String, SqlQuery]
  final lazy val select = s"SELECT ${fields.map(f => s"${tableName}.${f}").mkString(", ")} FROM $tableName"
  final lazy val deleteSql = SQL(s"DELETE FROM $tableName where id = {id}")
  final lazy val insertSql = SQL(
    s"""INSERT INTO $tableName(${fields.tail.mkString(", ")})
        VALUES( ${fields.tail.map{f => s"{${toVariableCase(f)}}"}.mkString(", ")})
     """
  )

  final lazy val updateSql = SQL(
    s"""UPDATE $tableName SET ${fields.tail.map(f => s"$f = {${toVariableCase(f)}}").mkString(", ")}
        WHERE id = {id}
     """
  )

  def byId(id: Long): Option[T] = DB.withConnection { implicit c =>
    byFieldsFirstOpt('id -> id)
  }

  def delete(id: Long): Unit = DB.withConnection { implicit c =>
    deleteSql
      .on('id -> id)
      .executeUpdate
  }

  def all: Seq[T] = DB.withConnection{ implicit c =>
    byFieldsList()
  }

  protected def byFieldsFirstOpt(args: NamedParameter*) = DB.withConnection{ implicit c =>
     byFields(args:_*).as(parser singleOpt)
  }

  protected def byFieldsList(args: NamedParameter*): List[T] = DB.withConnection{ implicit c =>
    byFields(args:_*).as(parser *)
  }

  private def byFields(args: NamedParameter*): SimpleSql[Row] = DB.withConnection{ implicit c =>
    val condition = args.toSeq.map{ a => s"""${a.name} = {${a.name}}"""}.mkString(" AND ")
      queries.find(p => p._1 == condition).map(_._2).getOrElse{
        val query = if (condition.isEmpty) SQL(select) else SQL(s"""$select WHERE $condition""")
        queries = queries + (condition -> query)
        query
      }.on(args:_*)
  }

  protected def update(entity: T): T
  protected def insert(entity: T): T

  def toVariableCase(s: String) =  {
      val camelCase = toCamelCase(s)
      camelCase.head.toLower + camelCase.tail
    }

  private def toProperCase(s: String): String = {
    if (s == null || s.trim.size == 0) ""
    else s.substring(0, 1).toUpperCase() + s.substring(1).toLowerCase()
  }

  private def toCamelCase(s: String): String = s.split("_").foldLeft("") {
    (camelCaseString, part) =>
      camelCaseString + toProperCase(part)
  }
}
