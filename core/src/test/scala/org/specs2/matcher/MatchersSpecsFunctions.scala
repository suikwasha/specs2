package org.specs2

import matcher.MustExpectations._
import matcher.StringMatchers._
import execute.{ResultExecution, Result}

/**
 * Those definitions help specifying the result messages for matchers
 */
package object matcher {

  implicit class Returns[T](t: =>MatchResult[T]) {
    def returns(m: String) = ResultExecution.execute(t.toResult) must contain(m) ^^ { (m: Result) => m.message }
    def returnsMatch(m: String) = ResultExecution.execute(t.toResult) must beMatching(m) ^^ { (m: Result) => m.message }
    def returnsResult(m: String) = {
      lazy val r = t.toResult
      ResultExecution.execute(r) must contain(m) ^^ { (m: Result) => if (r.isSuccess) "success: "+m.message else "failure: "+m.message   }
    }
  }
}
