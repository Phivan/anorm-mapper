package controllers

import java.sql.SQLException

import play.api._
import play.api.db.DB
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import services.ModelExtractor
import services.ModelExtractor.{Table, Column}

import scala.util.{Failure, Success, Try}

object Application extends Controller {
/*
  case class Generate(
    db: Option[String] = Some("jdbc:mysql://localhost/my-db?useUnicode=yes&characterEncoding=UTF-8")
    ,userName: Option[String] = None
    ,password: Option[String] = None
    ,table: Option[String] = None
    ,caseClassName: Option[String] = None
    ,packageName: Option[String] = Some("models")
  )

  val generateForm = Form(
  mapping(
    "db" -> optional(text),
    "userName" -> optional(text),
    "password" -> optional(text),
    "table" -> optional(text),
    "caseClassName" -> optional(text),
    "packageName" -> optional(text)
  )(Generate.apply)(Generate.unapply)
)

  def index = Action { implicit r =>
    Ok(views.html.index(
      generateForm.fill(Generate())
    ))
  }
  def generate= Action { implicit r =>
    val form = generateForm.bindFromRequest()
      form.fold(
      error => BadRequest
      ,generate => {
          (for{
            db <- generate.db
            userName <- generate.userName
            password <- generate.password
            table <- generate.table
            } yield {
              ModelExtractor.extract(db, userName, password, table).map{ t =>
                val generated = ModelExtractor.generate(t, generate.caseClassName, generate.packageName)
                Ok(views.html.index(form, Some(generated)))
              }.getOrElse(
                  BadRequest(views.html.index(form)).flashing("errors" -> "Cannot connect to database")
              )
          }).getOrElse(
            BadRequest(views.html.index(form)).flashing("errors" -> "Missing required fields")
          )
        }
    )
  }
  */
  def index() = Action {implicit r =>
    Ok(views.html.index.render("", None))
  }


  def generate() = Action{ implicit r =>
    Form(
      single(
        "input" -> nonEmptyText
      )
    ).bindFromRequest.fold(
        errors => {BadRequest}
        ,q => {extract(q)}
      )

  }


  def extract(q: String) = {
    import play.api.Play.current

    val extractName = "(?is)\\s*CREATE\\s*TABLE\\s*(.*?)\\s*\\(.*".r
    q.replaceAll("`", "") match{
      case url @ extractName(name) => {
        DB.withConnection{ implicit c =>
          Try{
            val st = c.createStatement();
            st.execute("SET FOREIGN_KEY_CHECKS=0")
            st.execute(q)
            val columns = c.getMetaData.getColumns(null ,null, name, "%")
            var seq = scala.collection.mutable.Seq.empty[Column]
            while(columns.next()){
              seq = seq:+ Column(
                columns.getString("COLUMN_NAME"),
                ModelExtractor.rawTypeInScala(columns.getInt("DATA_TYPE")),
                columns.getString("IS_NULLABLE") == "YES"
              )
            }
            st.execute(s"DROP TABLE $name")
            st.execute("SET FOREIGN_KEY_CHECKS=1")
            val t = Table(name, seq.toArray.toSeq)
            (ModelExtractor.generate(
              t, None, None
            ), ModelExtractor.generateWithModel(
              t, None, None
            ))

          } match {
            case Success(t) => {Ok(views.html.index.render(q, Some(t)))}
            case Failure(e) => {
              e.printStackTrace()
              BadRequest("Exeption thrown")}
          }
        }
      }
      case _ => {BadRequest("Impossible de parser ")}
    }
  }
}