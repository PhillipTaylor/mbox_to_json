package uk.gov.homeoffice.bsonreader

import cats.data.*
import cats.effect.*
import cats.effect.implicits.{*, given}
import cats.effect.unsafe.implicits.global
import cats.implicits.{*, given}

import com.typesafe.config.*

import fs2.io.file._

import scala.util.*
import scopt.OParser

import io.circe.*
import io.circe.syntax.*
import org.jsoup.Jsoup

sealed trait EmailElement
case class BeginMarker(line :String) extends EmailElement
case class Header(key :String, value :String) extends EmailElement
case class TextLine(text :String) extends EmailElement

object MainApp extends IOApp:

  def run(args: List[String]): IO[ExitCode] = {
    val inputFile = args(0)
    val outputFile = args(1)
    val pandasMode = args.contains("--pandas")
    
    val fout = new java.io.FileOutputStream(outputFile)

    if (!pandasMode) { fout.write("[\n".getBytes) }

    val writeJson :(Json => Unit) = { json =>
      val shortJs = Option(json.dropNullValues)
      (shortJs, pandasMode) match {
        case (Some(js), _) if js == null =>
        case (Some(js), true) => fout.write(js.noSpaces.getBytes)
        case (Some(js), false) => fout.write(js.spaces4.getBytes)
        case _ =>
      }
      fout.write("\n".getBytes)
    }

    Files[IO].readAll(Path(inputFile))
      .through(fs2.text.utf8.decode)
      .through(fs2.text.lines)
      .groupAdjacentBy { _.matches("^From .*") }
      .filter { !_._1 }
      .map { _._2.toList.mkString("\n") }
      .map { l => Email(l) }
      .map { email => email.toJson() }
      .map { json => Try(writeJson(json)) }
      .compile
      .drain
      .map { _ =>
        if (!pandasMode) { fout.write("\n]".getBytes) }
        fout.close()
      }
      .as(ExitCode(0))
  }

case class Email(emailRaw :String):

  import org.apache.commons.mail.util.*
  import javax.mail.internet.MimeMessage
  import javax.mail.Session
  import java.io.ByteArrayInputStream
  import scala.jdk.CollectionConverters.*

  val session = Session.getInstance(new java.util.Properties()) 

  def toJson() :Json = {

    Try { 
      val mimeMessage = new MimeMessage(session, new ByteArrayInputStream(emailRaw.getBytes))
      val email = new MimeMessageParser(mimeMessage)
      email.parse()
      (mimeMessage, email)
    }.toOption.map { case (mimeMessage, email) =>
      val dt = ""
      val from = Try(email.getFrom()).toOption.getOrElse("")
      val to = Try(email.getTo().iterator().asScala.map(_.toString).mkString(";")).toOption.getOrElse("")
      val subject = Try(email.getSubject()).toOption.getOrElse("")
      val text = email.getPlainContent()
      val html = email.getHtmlContent()
      val attachmentList = email.getAttachmentList()

      val content = (email.hasPlainContent(), email.hasHtmlContent()) match {
        case (true, _) => email.getPlainContent()
        case (false, true) =>
          val htmlContent = email.getHtmlContent()
          val text = Jsoup.parse(htmlContent).text()
          text
        case (false, false) => ""
      }

      val headers = mimeMessage.getAllHeaders().asScala.toList
      def getHeader(header :String) :List[String] = headers.filter(_.getName() == header).map { hdr => hdr.getValue() }
      def getJsonHeader(header :String) = Json.fromString(getHeader(header).headOption.getOrElse(""))
      def hasGmailLabel(label :String) = if (getHeader("X-Gmail-Labels").contains(label)) Json.fromBoolean(true) else Json.fromBoolean(false)

      Json.obj(
        "from" -> Json.fromString(from),
        "to" -> Json.fromString(to),
        "isSpam" -> hasGmailLabel("Spam"),
        "isSent" -> hasGmailLabel("Sent"),
        "date" -> getJsonHeader("Date"),
        "messageID" -> getJsonHeader("Message-Id"),
        "content" -> Json.fromString(content)
      )

    }.getOrElse(Json.obj("error" -> Json.fromString("failed to parse this message")))
  }
