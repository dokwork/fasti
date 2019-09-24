package ru.dokwork.fasti.example

import cats.MonadError
import cats.implicits._
import io.circe
import io.circe.Json
import io.circe.generic.JsonCodec
import ru.dokwork.fasti.Saga
import ru.dokwork.fasti.persistence._
import shapeless._

object BuyingItemSaga {

  case class BuyItem(item: Item)

  @JsonCodec
  case class DeferredOrder(orderId: OrderId, item: Item)

  @JsonCodec
  case class CreditReserved(orderId: OrderId, id: CreditId, item: Item)

  @JsonCodec
  case class ItemFound(orderId: OrderId, code: ItemCode, item: Item)

  @JsonCodec
  case class DeliveryOrdered(orderId: OrderId, deliveryId: DeliveryId)

  case class Done(orderId: OrderId)

  implicit def encode[A: circe.Encoder]: Encoder[A, Json] = a => circe.Encoder[A].apply(a)

  type States = DeferredOrder :: CreditReserved :: ItemFound :: DeliveryOrdered :: HNil

  def apply[F[_]: MonadError[*[_], Throwable]: SagaPersistence[*[_], Json, OrderId]](
      orderService: OrderService[F],
      billingService: BillingService[F],
      storeService: StoreService[F],
      deliveryService: DeliveryService[F]
  ): CompletedPersistedSaga[F, BuyItem, Done, States, OrderId, Json] = {

    type Step[A, B] = PersistedSaga[F, A, B, HNil, OrderId, Json]

    val createOrder: Step[BuyItem, DeferredOrder] = PersistedSaga(
      (args: BuyItem) => orderService.createOrder(args.item).map(DeferredOrder(_, args.item)),
      (order: DeferredOrder, cause: Throwable) => orderService.markAsFailed(order.orderId, cause)
    )(_.orderId)

    val reserveCredit: Step[DeferredOrder, CreditReserved] = PersistedSaga(
      (order: DeferredOrder) =>
        billingService.reserveCredit(order.item.price).map(CreditReserved(order.orderId, _, order.item)),
      (credit: CreditReserved, _: Throwable) => billingService.returnCredit(credit.id)
    )(_.orderId)

    val findItem: Step[CreditReserved, ItemFound] = PersistedSaga(
      (credit: CreditReserved) => storeService.findItem(credit.item).map(ItemFound(credit.orderId, _, credit.item)),
      (item: ItemFound, _: Throwable) => storeService.revertItem(item.code)
    )(_.orderId)

    val orderDelivery: Step[ItemFound, DeliveryOrdered] = PersistedSaga(
      (item: ItemFound) => deliveryService.orderDelivery(item.item).map(DeliveryOrdered(item.orderId, _)),
      (deliveryOrder: DeliveryOrdered, _: Throwable) => deliveryService.cancelOrder(deliveryOrder.deliveryId)
    )(_.orderId)

    val completeOrder = Saga(
      (deliveryOrdered: DeliveryOrdered) =>
        orderService.markAsCompleted(deliveryOrdered.orderId).as(Done(deliveryOrdered.orderId))
    )

    createOrder andThen reserveCredit andThen findItem andThen orderDelivery completeOn completeOrder
  }
}
