
package code
package snippet

import net.liftweb._
import http._
import common._
import util.Helpers._
import scala.xml.NodeSeq

import net.liftweb.http._
import net.liftweb.http.SHtml._
import net.liftweb.util._
import net.liftweb.util.Helpers._
import net.liftweb.common._

import java.net.{HttpURLConnection, MalformedURLException, URL, URLEncoder}
import java.io.{BufferedReader, InputStreamReader, IOException, OutputStreamWriter}


class Stateful extends StatefulSnippet {

  
  val privateKey = "6Le1lb4SAAAAAJXXHBc_ebosxBOBIBy8sJUJyIUR"
  var email = "Default value"
  var challenge = ""
  var response = ""
  
  private val whence = S.referer openOr "/"
  
  def dispatch = {case "render" => render}
  
  def render =
    "name=email"     #> SHtml.text(email, (x:String) => email=x) &
    "name=challenge" #> SHtml.hidden( (x:String)=>challenge=x, "" ) &
    "name=response"  #> SHtml.hidden( (x:String)=>response=x, "" ) &
    "type=submit"    #> SHtml.submit("Send", () => {
      if (verifyCaptcha(challenge, response)) {
        submitAction(email)
        println("\n\n...............................................................OK!!!!!!!!!!!!!!\n\n")
      } else {
        S.error("Recaptcha input not correct")
      }
    })
    

  def submitAction(email: String) { /*processing*/ }

  def verifyCaptcha(challenge: String, response: String) : Boolean = {
     
    val url = new URL("http://www.google.com/recaptcha/api/verify")
    val con = url.openConnection.asInstanceOf[HttpURLConnection]
    con.setDoOutput(true)
    con.setRequestMethod("POST")
    val writer = new OutputStreamWriter(con.getOutputStream)
    writer.write("privatekey=" + privateKey)
    writer.write("&remoteip=" + S.containerRequest.map(_.remoteAddress).openOr("localhost"))
    writer.write("&challenge=" + challenge)
    writer.write("&response=" + response)
    writer.close

    if (con.getResponseCode == HttpURLConnection.HTTP_OK) {
      val reader = new BufferedReader(new InputStreamReader(con.getInputStream))
      val res : Boolean = reader.readLine match {
        case "true" => true
        case _ => false
      }
      reader.close
      res
    }
    else {
      sys.error("Connection problem to reCaptcha")
      false	
    }
  }  
  
}
