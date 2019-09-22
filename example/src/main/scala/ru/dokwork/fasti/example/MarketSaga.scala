package ru.dokwork.fasti.example

import cats.MonadError
import cats.implicits._
import io.circe
import io.circe.Json
import io.circe.generic.JsonCodec
import ru.dokwork.fasti.Saga
import ru.dokwork.fasti.persistence._
import shapeless._

object MarketSaga {

  case class MakeOrderArgs(item: Item)

  @JsonCodec
  case class DeferredOrder(orderId: OrderId, item: Item)

  @JsonCodec
  case class CreditReserved(orderId: OrderId, id: CreditId, item: Item)

  @JsonCodec
  case class ItemFound(orderId: OrderId, code: ItemCode, item: Item)

  @JsonCodec
  case class DeliveryOrdered(orderId: OrderId, deliveryId: DeliveryId)

  case class Done(orderId: OrderId)

  implicit def encode[A: circe.Encoder]: Encoder[A, Json] = a ⇒ circe.Encoder[A].apply(a)

  type States = DeferredOrder :: CreditReserved :: ItemFound :: DeliveryOrdered :: HNil

  def apply[F[_] : MonadError[*[_], Throwable] : SagaPersistence[*[_], Json, OrderId]](
    orderService: OrderService[F],
    billingService: BillingService[F],
    storeService: StoreService[F],
    deliveryService: DeliveryService[F]
  ): CompletedPersistedSaga[F, MakeOrderArgs, Done, States, OrderId, Json] = {

    val createOrder = PersistedSaga[OrderId, Json](
      (args: MakeOrderArgs) ⇒ orderService.createOrder(args.item).map(DeferredOrder(_, args.item)),
      (order: DeferredOrder, cause: Throwable) ⇒ orderService.markAsFailed(order.orderId, cause)
    )(_.orderId)

    val reserveCredit = PersistedSaga[OrderId, Json](
      (order: DeferredOrder) ⇒ billingService.reserveCredit(order.item.price).map(CreditReserved(order.orderId, _, order.item)),
      (credit: CreditReserved, _: Throwable) ⇒ billingService.returnCredit(credit.id)
    )(_.orderId)

    val findItem = PersistedSaga[OrderId, Json](
      (credit: CreditReserved) ⇒ storeService.findItem(credit.item).map(ItemFound(credit.orderId, _, credit.item)),
      (item: ItemFound, _: Throwable) ⇒ storeService.revertItem(item.code)
    )(_.orderId)

    val orderDelivery = PersistedSaga[OrderId, Json](
      (item: ItemFound) ⇒ deliveryService.orderDelivery(item.item).map(DeliveryOrdered(item.orderId, _)),
      (deliveryOrder: DeliveryOrdered, _: Throwable) ⇒ deliveryService.cancelOrder(deliveryOrder.deliveryId)
    )(_.orderId)

    val completeOrder = Saga(
      (deliveryOrdered: DeliveryOrdered) ⇒ orderService.markAsCompleted(deliveryOrdered.orderId).as(Done(deliveryOrdered.orderId))
    )

    createOrder andThen reserveCredit andThen findItem andThen orderDelivery completeOn completeOrder
  }
}





