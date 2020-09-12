package com.abstractcode.runnerexample.payment

import java.util.UUID

import com.abstractcode.eventrunner.{MessageContainer, Metadata}
import com.abstractcode.runnerexample.payment.PaymentMessage.{PaymentMessageType, PaymentTransactionId}

sealed trait PaymentMessage

case class PaymentMetadata(transactionId: PaymentTransactionId, messageType: PaymentMessageType)
  extends Metadata[PaymentTransactionId, PaymentMessageType]

object PaymentMessage {
  type PaymentTransactionId = UUID
  type PaymentMessageType = String
  type PaymentMessageContainer = MessageContainer[PaymentMessage, PaymentMetadata]

  case class UserId(id: UUID)

  sealed trait Currency
  case object AustralianDollar extends Currency
  case object Euro extends Currency
  case object NewZealandDollar extends Currency

  case class Money(amount: BigDecimal, currency: Currency)

  case class TakePaymentMessage(userId: UserId, encryptedPaymentDetails: String, amount: Money) extends PaymentMessage
  case class ReversePaymentMessage(userId: UserId) extends PaymentMessage
}
