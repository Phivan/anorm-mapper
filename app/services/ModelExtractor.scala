package services

import java.sql.Connection

import com.jolbox.bonecp.BoneCPDataSource


import scala.util.Try

object ModelExtractor {

  def generateWithModel(table: Table, maybeClassName: Option[String], maybePackageName: Option[String]) = {
    val className = maybeClassName.getOrElse(toCamelCase(table.tableName))
    val packageName = maybePackageName.getOrElse("models")
    val fields = table.columns.map(_.toCaseClassString).mkString(",\n")
    val dataFields = table.columnsWithoutId.map(_.toCaseClassString).mkString(",\n")
    val imports = new StringBuilder("import play.api.libs.json.Json\nimport anorm._\nimport anorm.SqlParser._\n")
    table.columns.find(_.rawType == "DateTime").map{ t =>
      imports.append("import org.joda.time.DateTime\n")
    }
    val form = if(table.columns.forall(c => c.formName.isDefined)){
        val sb = new StringBuilder(s"""\n\n  val defaultForm = Form(\n    mapping(\n""")
        sb.append(table.columns.map{ c =>
          if(c.nullable){
            s"""      \"${c.variableName}\" -> optional(${c.formName.get}) """
          }else{
            s"""      \"${c.variableName}\" -> ${c.formName.get} """
          }
        }.mkString(",\n")).append(s"\n    )(${className}.apply)(${className}.unapply)\n  )\n")
      Some(sb.toString)
    }else{None}

    form.map{f =>
      imports.append("import play.api.data._\nimport play.api.data.Forms._\nimport play.api.data.format.Formats._\n")
    }

    val rawMapper = s"""  override val parser: RowParser[$className] = {\n""" ++
                    table.columns.map(c => s"""      get[${c.toRawTypeString}](\"${table.tableName}.${c.name}\")"""). mkString(" ~\n") ++
                    s""" map {
                    |         case ${table.columns.map(c => c.variableName).mkString("~")} => ${className}(${table.columns.map(c => c.variableName).mkString(", ")})
                    |      }
                    |  }
                     """.stripMargin

    val mapper = table.columns.map(c => s"""    r[${c.toRawTypeString}]("${c.name}")""").mkString(",\n")


    val variableClass= toCamelCaseLowerFirst(className)
    val insertOn = table.columnsWithoutId.map{c => s"""      '${c.variableName} -> ${variableClass}.${c.variableName}"""}.mkString(",\n")
    val updateOn = table.columns.map{c => s"""      '${c.variableName} -> ${variableClass}.${c.variableName}"""}.mkString(",\n")
    /* private val allSql = SQL(
    """SELECT id, name, sort, disabled from category order by sort"""
  )*/

    val sb = new StringBuilder
    sb.append(s"""
          |package $packageName
          |$imports
          |case class $className(
          |$fields
          |){
          |  def save(): $className  = {
          |    id match{
          |      case -1 => ${className}.insert(this)
          |      case _ => ${className}.update(this)
          |    }
          |  }
          |
          |  def delete(): Unit = {
          |    ${className}.delete(id)
          |  }
          |}
          |
          |object $className extends Model[$className]{
          |   implicit val _format = Json.format[$className]
          |   override val fields: Seq[String] = Seq(${table.columnNames})
          |   override val tableName: String = \"${table.tableName}"
          |
          |$rawMapper
          |
          |   override def insert($variableClass: ${className})(implicit c: java.sql.Connection): $className = {
          |    val generatedId = insertSql.on(
          |$insertOn
          |    ).executeInsert(SqlParser.scalar[Long] single)
          |    $variableClass.copy(id = generatedId)
          |  }
          |
          |   override  def update($variableClass: ${className})(implicit c: java.sql.Connection): $className  = {
          |    updateSql.on(
          |$updateOn
          |    ).executeUpdate
          |    $variableClass
          |  }${form.getOrElse("")}
          |}
          |
          |""".stripMargin)
    sb.toString
  }

  def generate(table: Table, maybeClassName: Option[String], maybePackageName: Option[String]) = {
    val className = maybeClassName.getOrElse(toCamelCase(table.tableName))
    val packageName = maybePackageName.getOrElse("models")
    val fields = table.columns.map(_.toCaseClassString).mkString(",\n")
    val dataFields = table.columnsWithoutId.map(_.toCaseClassString).mkString(",\n")
    val imports = new StringBuilder("import play.api.libs.json.Json\nimport anorm._\nimport play.api.db.DB\nimport play.api.Play.current\n")
    table.columns.find(_.rawType == "DateTime").map{ t =>
      imports.append("import org.joda.time.DateTime\n")
    }
    val form = if(table.columns.forall(c => c.formName.isDefined)){
        val sb = new StringBuilder(s"""\n\n  val defaultForm = Form(\n    mapping(\n""")
        sb.append(table.columns.map{ c =>
          if(c.nullable){
            s"""      \"${c.variableName}\" -> optional(${c.formName.get}) """
          }else{
            s"""      \"${c.variableName}\" -> ${c.formName.get} """
          }
        }.mkString(",\n")).append(s"\n    )(${className}.apply)(${className}.unapply)\n  )\n")
      Some(sb.toString)
    }else{None}

    form.map{f =>
      imports.append("import play.api.data._\nimport play.api.data.Forms._\nimport play.api.data.format.Formats._\n")
    }

    val mapper = table.columns.map(c => s"""    r[${c.toRawTypeString}]("${c.name}")""").mkString(",\n")


    val variableClass= toCamelCaseLowerFirst(className)
    val insertOn = table.columnsWithoutId.map{c => s"""      '${c.variableName} -> ${variableClass}.${c.variableName}"""}.mkString(",\n")
    val updateOn = table.columns.map{c => s"""      '${c.variableName} -> ${variableClass}.${c.variableName}"""}.mkString(",\n")
    /* private val allSql = SQL(
    """SELECT id, name, sort, disabled from category order by sort"""
  )*/

    val sb = new StringBuilder
    sb.append(
      s"""
          |package $packageName
          |$imports
          |case class $className(
          |$fields
          |){
          |  def save(): $className  = {
          |    id match{
          |      case -1 => ${className}.insert(this)
          |      case _ => ${className}.update(this)
          |    }
          |  }
          |
          |  def delete(): Unit = {
          |    ${className}.delete(id)
          |  }
          |}
          |
          |
          |object $className{
          |  implicit val format = Json.format[$className]
          |  private val fields = Seq(${table.columnNames})
          |  private val select = s"SELECT $${fields.mkString(", ")} FROM ${table.tableName}"
          |  private val allSql = SQL(select)
          |  private val byIdSql = SQL(s"$$select WHERE id = {id}")
          |  private val deleteSql = SQL("DELETE FROM ${table.tableName} where id = {id}")
          |  private val insertSql = SQL(\"\"\"
          |    INSERT INTO ${table.tableName}(${table.columnsWithoutId.map(_.name).mkString(", ")})
          |    VALUES(${table.columnsWithoutId.map(c => s"{${c.variableName}}").mkString(", ")})
          |  \"\"\")
          |  private val updateSql = SQL(\"\"\"
          |    UPDATE ${table.tableName} SET ${table.columnsWithoutId.map(c => s"${c.name} = {${c.variableName}}" ).mkString(", ")}
          |    WHERE id = {id}
          |  \"\"\")
          |
          |  private def bind(r: Row) = $className(
          |$mapper
          |  )
          |
          |  def all: Seq[$className] = DB.withConnection{ implicit c =>
          |    allSql.map(bind).list()
          |  }
          |
          |  def byId(id: Long): Option[$className] = DB.withConnection { implicit c =>
          |    byIdSql
          |     .on('id -> id)
          |     .map(bind)
          |     .singleOpt()
          |  }
          |
          |  protected def delete(id: Long): Unit = DB.withConnection { implicit c =>
          |    deleteSql
          |      .on('id -> id)
          |      .executeUpdate
          |  }
          |
          |  protected def insert($variableClass: ${className}): $className = DB.withConnection { implicit c =>
          |    val generatedId = insertSql.on(
          |$insertOn
          |    ).executeInsert(SqlParser.scalar[Long] single)
          |    $variableClass.copy(id = generatedId)
          |  }
          |
          |  protected def update($variableClass: ${className}): $className  = DB.withConnection { implicit c =>
          |    updateSql.on(
          |$updateOn
          |    ).executeUpdate
          |    $variableClass
          |  }${form.getOrElse("")}
          |}
      """.stripMargin)
      sb.toString
  }

  val defaultModel =
    s"""

     """.stripMargin

  private def toCamelCase(s: String): String = s.split("_").foldLeft("") {
    (camelCaseString, part) =>
      camelCaseString + toProperCase(part)
  }

  def toCamelCaseLowerFirst(s: String) =  {
      val camelCase = toCamelCase(s)
      camelCase.head.toLower + camelCase.tail
    }

  private def toProperCase(s: String): String = {
    if (s == null || s.trim.size == 0) ""
    else s.substring(0, 1).toUpperCase() + s.substring(1).toLowerCase()
  }

  def extract(db: String, username: String, password: String, table: String) = {
    Try{
      val datasource = new BoneCPDataSource()
      datasource.setJdbcUrl(db)
      datasource.setLazyInit(true)
      datasource.setUsername(username)
      datasource.setPassword(password)
      datasource.setConnectionTestStatement("SELECT 1")
      datasource.setDriverClass("com.mysql.jdbc.Driver")
      val connection: Connection = datasource.getConnection(username, password);
      val columns = connection.getMetaData.getColumns(null ,null, table, "%")
      var seq = scala.collection.mutable.Seq.empty[Column]
      while(columns.next()){
        seq = seq:+ Column(
          columns.getString("COLUMN_NAME"),
          rawTypeInScala(columns.getInt("DATA_TYPE")),
          columns.getString("IS_NULLABLE") == "YES"
        )
      }
      Table(table, seq.toArray.toSeq)
    }.toOption
  }

  case class Table(
    tableName: String,
    columns: Seq[Column]
  ){
    def id = columns.filter(_.name == "id")
    def columnsWithoutId= columns.filterNot(_.name == "id")

    def columnNames = columns.map{ c => s""""${c.name}""""}.mkString(", ")
  }

  case class Column(name: String, rawType: String, nullable: Boolean){
    def toCaseClassString = {
      s"  $variableName: $toRawTypeString"
    }
    
    def variableName = toCamelCaseLowerFirst(name)
    
    def toRawTypeString = {
      if(nullable) s"Option[$rawType]"
      else rawType
    }

    def formName = {
      import play.api.data._
      import play.api.data.Forms._
      number
      rawType match{
       case TypeName.Long =>       Some("longNumber")
       case TypeName.Int =>        Some("number")
       case TypeName.Boolean =>    Some("boolean")
       case TypeName.BigDecimal => Some("bigDecimal")
       case TypeName.String =>     Some("nonEmptyText")
       case TypeName.DateTime =>   Some("jodaDate")
       case _ =>     Some(s"of[$rawType]")
      }
    }
  }

  object TypeName {
    val Long = "Long"
    val Boolean = "Boolean"
    val BigDecimal = "BigDecimal" // scala.math.BigDecimal
    val String = "String"
    val Byte = "Byte"
    val Int = "Int"
    val Short = "Short"
    val Float = "Float"
    val TimeStamp = "DateTime" // scala.math.BigDecimal
    val DateTime = "DateTime"

    val Double = "Double"
    val Blob = "Blob"
    val Clob = "Clob"
    val Ref = "Ref"
    val Struct = "Struct"
    val LocalDate = "LocalDate"
    val LocalTime = "LocalTime"
    val Any = "Any"
    val AnyArray = "Array[Any]"
    val ByteArray = "Array[Byte]"
  }
  def rawTypeInScala(i: Int): String = {

    import java.sql.{ Types => JavaSqlTypes }
      i match {

      case JavaSqlTypes.ARRAY => TypeName.AnyArray
      case JavaSqlTypes.BIGINT => TypeName.Long
      case JavaSqlTypes.BINARY => TypeName.ByteArray
      case JavaSqlTypes.BIT => TypeName.Boolean
      case JavaSqlTypes.BLOB => TypeName.Blob
      case JavaSqlTypes.BOOLEAN => TypeName.Boolean
      case JavaSqlTypes.CHAR => TypeName.String
      case JavaSqlTypes.CLOB => TypeName.Clob
      case JavaSqlTypes.DATALINK => TypeName.Any
      case JavaSqlTypes.DATE => TypeName.LocalDate
      case JavaSqlTypes.DECIMAL => TypeName.BigDecimal
      case JavaSqlTypes.DISTINCT => TypeName.Any
      case JavaSqlTypes.DOUBLE => TypeName.Double
      case JavaSqlTypes.FLOAT => TypeName.Float
      case JavaSqlTypes.INTEGER => TypeName.Int
      case JavaSqlTypes.JAVA_OBJECT => TypeName.Any
      case JavaSqlTypes.LONGVARBINARY => TypeName.ByteArray
      case JavaSqlTypes.LONGVARCHAR => TypeName.String
      case JavaSqlTypes.NULL => TypeName.Any
      case JavaSqlTypes.NUMERIC => TypeName.BigDecimal
      case JavaSqlTypes.OTHER => TypeName.Any
      case JavaSqlTypes.REAL => TypeName.Float
      case JavaSqlTypes.REF => TypeName.Ref
      case JavaSqlTypes.SMALLINT => TypeName.Short
      case JavaSqlTypes.STRUCT => TypeName.Struct
      case JavaSqlTypes.TIME => TypeName.LocalTime
      case JavaSqlTypes.TIMESTAMP => TypeName.TimeStamp
      case JavaSqlTypes.TINYINT => TypeName.Byte
      case JavaSqlTypes.VARBINARY => TypeName.ByteArray
      case JavaSqlTypes.VARCHAR => TypeName.String
      case _ => TypeName.Any
    }
  }
}
