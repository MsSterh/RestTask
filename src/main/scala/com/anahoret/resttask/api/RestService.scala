package com.anahoret.resttask
package api

import net.liftweb._
import common._
import util._
import Helpers._
import http.rest.RestHelper

import json.JObject
import json.JsonDSL._


object RestService extends RestHelper {
  serve {
    case "fib" :: AsInt(num) :: _ Get _ => ("response" -> fib(num)) : JObject
  }

  private def fib(n: Int) = {
    def fibTR(n: Int, next: Int, res: Int): Int = n match {
      case 0 => res
      case _ => fibTR(n - 1, res + next, next)
    }

    fibTR(n, 1, 0)
  }
}
