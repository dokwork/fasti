package ru.dokwork.fasti.example

import derevo.derive
import derevo.circe.magnolia.{decoder, encoder}

@derive(decoder, encoder)
case class OrderId(id: Long)

@derive(decoder, encoder)
case class ItemCode(code: String)

@derive(decoder, encoder)
case class CreditId(id: Long)

@derive(decoder, encoder)
case class DeliveryId(id: Long)

@derive(decoder, encoder)
case class Item(name: String, price: BigDecimal)