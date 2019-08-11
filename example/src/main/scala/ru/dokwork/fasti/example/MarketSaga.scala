package ru.dokwork.fasti.example

import cats.MonadError
import cats.implicits._
import io.circe.{ Encoder, Json }
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

  case class Done(orderId: OrderId) extends Result

  implicit def encode[A: Encoder]: Encoder[A, Json] = a ⇒ Encoder[A].apply(a)

  implicit val lensDeferredOrder: Lens[DeferredOrder, OrderId] = ???
  implicit val lensCreditReserved: Lens[CreditReserved, OrderId] = ???
  implicit val lensItemFound: Lens[ItemFound, OrderId] = ???
  implicit val lensDeliveryOrdered: Lens[DeliveryOrdered, OrderId] = ???
  implicit val lensDone: Lens[Done, OrderId] = ???

  type States = DeferredOrder :: CreditReserved :: ItemFound :: DeliveryOrdered :: HNil

  def apply[F[_] : MonadError[?[_], Throwable] : SagaPersistence[?[_], Json, OrderId]](
    orderService: OrderService[F],
    billingService: BillingService[F],
    storeService: StoreService[F],
    deliveryService: DeliveryService[F]
  ): Saga[F, MakeOrderArgs, Done, States] = {

    val createOrder = PersistedSaga(
      (args: MakeOrderArgs) ⇒ orderService.createOrder(args.item).map(DeferredOrder(_, args.item)),
      (order: DeferredOrder, cause: Throwable) ⇒ orderService.markAsFailed(order.orderId, cause)
    )

    val reserveCredit = PersistedSaga(
      (order: DeferredOrder) ⇒ billingService.reserveCredit(order.item.price).map(CreditReserved(order.orderId, _, order.item)),
      (credit: CreditReserved, _: Throwable) ⇒ billingService.returnCredit(credit.id)
    )

    val findItem = PersistedSaga(
      (credit: CreditReserved) ⇒ storeService.findItem(credit.item).map(ItemFound(credit.orderId, _, credit.item)),
      (item: ItemFound, _: Throwable) ⇒ storeService.revertItem(item.code)
    )

    val orderDelivery = PersistedSaga(
      (item: ItemFound) ⇒ deliveryService.orderDelivery(item.item).map(DeliveryOrdered(item.orderId, _)),
      (deliveryOrder: DeliveryOrdered, _: Throwable) ⇒ deliveryService.cancelOrder(deliveryOrder.deliveryId)
    )

    val completeOrder = PersistedSaga(
      (deliveryOrdered: DeliveryOrdered) ⇒ orderService.markAsCompleted(deliveryOrdered.orderId).as(Done(deliveryOrdered.orderId))
    )

    createOrder andThen reserveCredit andThen findItem andThen orderDelivery andThen completeOrder
  }
}





