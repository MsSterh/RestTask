package com.anahoret.resttask
package api

import net.liftweb._
import common._
import util._
import Helpers._
import http.rest.RestHelper

import json.{JObject, JValue}
import json.JsonDSL._


object RestService extends RestHelper {
  serve {
    case "fib" :: AsInt(num) :: _ Get _ => ("response" -> fib(num)) : JObject
    case "google-body" :: _ Get _ => ("response" -> Google.responseSHA) : JObject
    case "store" :: _ Post value  => Store.set(value.param("value").getOrElse("null")) : JValue
    case "store" :: _ Get _ => ("response" -> Store.get) : JObject
  }

  private def fib(n: Int) = {
    def fibTR(n: Int, next: Int, res: Int): Int = n match {
      case 0 => res
      case _ => fibTR(n - 1, res + next, next)
    }

    fibTR(n, 1, 0)
  }

  private object Google {
    import dispatch._
    import com.ning.http.client.AsyncHttpClientConfig

    val http = Http.configure { config: AsyncHttpClientConfig.Builder =>
      config.setFollowRedirects(true)
    }

    lazy val msgDigest = java.security.MessageDigest.getInstance("SHA-1")

    def responseBody: String = http(url("http://www.google.com") OK as.String)()

    def responseSHA: String = msgDigest.digest(responseBody.getBytes).map("%02x".format(_)).reduce(_ + _)
  }

  private object Store {
    import redis.clients.jedis._

    lazy val db: Jedis = new Jedis("localhost")

    def set(x: String) = db.set("value", x)
    def get: String = db.get("value")
  }

}
