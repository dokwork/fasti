package ru.dokwork.fasti.example

import io.circe.generic.JsonCodec

@JsonCodec
case class OrderId(id: Long)

@JsonCodec
case class ItemCode(code: String)

@JsonCodec
case class CreditId(id: Long)

@JsonCodec
case class DeliveryId(id: Long)

@JsonCodec
case class Item(name: String, price: BigDecimal)