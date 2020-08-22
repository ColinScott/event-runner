package com.abstractcode.runnerexample.payment

sealed trait PaymentMessage

object PaymentMessage {
  case class UserId(id: java.util.UUID)

  sealed trait Currency
  case object AustralianDollar extends Currency
  case object Euro extends Currency
  case object NewZealandDollar extends Currency

  case class Money(amount: BigDecimal, currency: Currency)

  case class TakePaymentMessage(userId: UserId, encryptedPaymentDetails: String, amount: Money) extends PaymentMessage
}
