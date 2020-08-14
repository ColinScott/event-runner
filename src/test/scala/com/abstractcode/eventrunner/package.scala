package com.abstractcode

import org.http4s.Uri
import org.scalacheck.Gen.freqTuple
import org.scalacheck.{Arbitrary, Gen}

package object eventrunner {
  val uriGen: Gen[Uri] = for {
    protocol <- Gen.frequency(List((5, "http://"), (10, "https://")).map(freqTuple): _*)
    uri <- Gen.identifier
    port <- Gen.chooseNum[Int](minT = 1, maxT = 65535)
  } yield Uri.unsafeFromString(s"$protocol$uri:$port")

  implicit val arbitraryUri: Arbitrary[Uri] = Arbitrary(uriGen)
}
