package exercise

class Shop[Item, Price: Numeric] {

    type PriceList = Item => Option[Price]
    type Entry = (Item, Option[Price])

    case class Bill(entries: List[Entry], total: Price)
    case class CheckoutFailure(issues: Set[Item], entries: List[Entry])

    val op = implicitly[Numeric[Price]]

    class Cashdesk(priceOf: PriceList) {

        def checkout(items: List[Item]): Either[CheckoutFailure, Bill] = {

            val entries: List[Entry] = items.map(item => (item, priceOf(item)))
            val totalOpt: Option[Price] = entries.map(_._2)
                .foldLeft(Option(op.zero))((so, po) => for (s <- so; p <- po) yield op.plus(s, p))

            totalOpt match {
                case Some(total) =>
                    Right(Bill(entries, total))

                case None =>
                    val issues = entries.collect { case (item, None) => item }.toSet
                    Left(CheckoutFailure(issues, entries))
            }
        }

    }
}

object AppleOrangeShop extends Shop[String, BigDecimal]

