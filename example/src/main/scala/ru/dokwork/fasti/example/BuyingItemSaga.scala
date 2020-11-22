package ru.dokwork.fasti.example

import cats.MonadError
import cats.implicits._
import derevo.circe.magnolia.{ decoder, encoder }
import derevo.derive
import io.circe._
import ru.dokwork.fasti.Saga
import ru.dokwork.fasti.persistence._
import shapeless.{ ::, HNil }

object BuyingItemSaga {

  @derive(decoder, encoder)
  case class BuyItem(item: Item)

  @derive(decoder, encoder)
  case class DeferredOrder(orderId: OrderId, item: Item)

  @derive(decoder, encoder)
  case class CreditReserved(orderId: OrderId, id: CreditId, item: Item)

  @derive(decoder, encoder)
  case class ItemFound(orderId: OrderId, code: ItemCode, item: Item)

  @derive(decoder, encoder)
  case class DeliveryOrdered(orderId: OrderId, deliveryId: DeliveryId)

  case class Done(orderId: OrderId)

  implicit def encode[A: Encoder]: Encode[A, Json] = a => Encoder[A].apply(a)

  type States = DeferredOrder :: CreditReserved :: ItemFound :: DeliveryOrdered :: HNil

  def apply[F[_]: MonadError[*[_], Throwable]: SagaPersistence[*[_], Json, OrderId]](
      orderService: OrderService[F],
      billingService: BillingService[F],
      storeService: StoreService[F],
      deliveryService: DeliveryService[F]
  ): CompletedPersistedSaga[F, BuyItem, Done, States, OrderId, Json] = {

    type Step[A, B] = PersistedSaga[F, A, B, HNil, OrderId, Json]

    val createOrder: Step[BuyItem, DeferredOrder] = PersistedSaga.create(
      (args: BuyItem) => orderService.createOrder(args.item).map(DeferredOrder(_, args.item)),
      (order: DeferredOrder, cause: Throwable) => orderService.markAsFailed(order.orderId, cause)
    )(_.orderId)

    implicitly[Encoder[Item]]
    val reserveCredit: Step[DeferredOrder, CreditReserved] = PersistedSaga.create(
      (order: DeferredOrder) =>
        billingService.reserveCredit(order.item.price).map(CreditReserved(order.orderId, _, order.item)),
      (credit: CreditReserved, _: Throwable) => billingService.returnCredit(credit.id)
    )(_.orderId)

    val findItem: Step[CreditReserved, ItemFound] = PersistedSaga.create(
      (credit: CreditReserved) => storeService.findItem(credit.item).map(ItemFound(credit.orderId, _, credit.item)),
      (item: ItemFound, _: Throwable) => storeService.revertItem(item.code)
    )(_.orderId)

    val orderDelivery: Step[ItemFound, DeliveryOrdered] = PersistedSaga.create(
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
