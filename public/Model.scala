package shared.models

import anorm._

/*
* Mysql generic Model
* */
trait Model[T] { self =>

  val fields: Seq[String]
  val tableName: String
  val parser: RowParser[T]
  protected var queries = Map.empty[String, SqlQuery]
  final lazy val select = s"SELECT ${fields.map(f => s"${tableName}.${f}").mkString(", ")} FROM $tableName"
  final lazy val selectDistinct = s"SELECT distinct ${fields.map(f => s"${tableName}.${f}").mkString(", ")} FROM $tableName"

  final lazy val insertSql = getSQL(
    s"""INSERT INTO $tableName(${fields.tail.mkString(", ")})
        VALUES( ${fields.tail.map{f => s"{${toVariableCase(f)}}"}.mkString(", ")})
     """
  )

  final lazy val updateSql = getSQL(
    s"""UPDATE $tableName SET ${fields.tail.map(f => s"$f = {${toVariableCase(f)}}").mkString(", ")}
        WHERE id = {id}
     """
  )


  def byId(id: Long)(implicit c:java.sql.Connection): Option[T] = {
    _byFieldsFirstOpt('id -> id)
  }

  def delete(id: Long)(implicit c:java.sql.Connection): Unit = {
    getSQL(s"DELETE FROM $tableName where id = {id}")
      .on('id -> id)
      .executeUpdate
  }

  def all(implicit c:java.sql.Connection): Seq[T] = {
    _byFieldsList()
  }

  def count(implicit c:java.sql.Connection): Long = _count()

  protected def _count(args: NamedParameter*)(implicit c:java.sql.Connection) = {
    val count = s"SELECT COUNT(*) as count from $tableName"
    val condition = args.toSeq.map{ a => s"""${a.name} = {${a.name}}"""}.mkString(" AND ")
    val selector = if (condition.isEmpty) count else s"""$count WHERE $condition"""
    getSQL(selector)
      .on(args: _*)
      .map(r => r[Long]("count"))
      .single()
  }

  protected def _byFieldsFirstOpt(args: NamedParameter*)(implicit c:java.sql.Connection) = {
    byFields(args:_*).as(parser singleOpt)
  }

  protected def _byFieldsList(args: NamedParameter*)(implicit c:java.sql.Connection): List[T] = {
    byFields(args:_*).as(parser *)
  }

  protected def getSQL(req: String): SqlQuery = {
    queries.find(p =>  p._1 == req).map(_._2).getOrElse{
      val query = SQL(req)
      queries = queries + (req -> query)
      query
    }
  }

  private def byFields(args: NamedParameter*)(implicit c:java.sql.Connection): SimpleSql[Row] = {
    val condition = args.toSeq.map{ a => s"""${a.name} = {${a.name}}"""}.mkString(" AND ")
    val selector = if (condition.isEmpty) select else s"""$select WHERE $condition"""
    getSQL(selector)
      .on(args:_*)
  }

  protected def update(entity: T): T
  protected def insert(entity: T): T


  def toVariableCase(s: String) =  {
    val camelCase = toCamelCase(s)
    camelCase.head.toLower + camelCase.tail
  }

  private def toProperCase(s: String): String = {
    if (s == null || s.trim.length == 0) ""
    else s.substring(0, 1).toUpperCase + s.substring(1).toLowerCase
  }

  private def toCamelCase(s: String): String = s.split("_").foldLeft("") {
    (camelCaseString, part) =>
      camelCaseString + toProperCase(part)
  }
}