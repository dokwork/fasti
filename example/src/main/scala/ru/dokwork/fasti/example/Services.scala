package ru.dokwork.fasti.example


trait OrderService[F[_]] {
  def createOrder: Item => F[OrderId]

  def markAsCompleted: OrderId => F[Unit]

  def markAsFailed: (OrderId, Throwable) => F[Unit]
}

trait BillingService[F[_]] {
  def reserveCredit: BigDecimal => F[CreditId]

  def returnCredit: CreditId => F[Unit]
}

trait StoreService[F[_]] {
  def findItem: Item => F[ItemCode]

  def revertItem: ItemCode => F[Unit]
}

trait DeliveryService[F[_]] {
  def orderDelivery: Item => F[DeliveryId]

  def cancelOrder: DeliveryId => F[Unit]
}
