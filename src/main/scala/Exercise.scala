package exercise

class Shop[Item, Price: Numeric] {

    type PriceList = Item => Option[Price]
    type Entry = (Item, Price)
    type SpecialOffer = (List[Entry], Price) => (List[Entry], Price)

    case class Bill(entries: List[Entry], total: Price)
    case class CheckoutFailure(issues: Set[Item], items: List[Item])

    val op = implicitly[Numeric[Price]]

    def sumTotal(entries: List[Entry]): Price = entries.map(_._2).foldLeft(op.zero)(op.plus)

    class Cashdesk(priceOf: PriceList, applyOffer: SpecialOffer = (e, t) => (e, t)) {

        def checkout(items: List[Item]): Either[CheckoutFailure, Bill] = {

            val result: Either[Set[Item], List[Entry]] = {
                val entries: List[(Item, Option[Price])] = items.map(item => (item, priceOf(item)))
                val successes: List[Entry] = entries.collect { case (i, Some(p)) => (i, p) }
                if (successes.size == entries.size) {
                    Right(successes)
                }
                else {
                    val issues: Set[Item] = entries.collect({ case (i, None) => i }).toSet
                    Left(issues)
                }
            }

            result match {
                case Right(entries) =>
                    val (es, t) = applyOffer(entries, sumTotal(entries))
                    Right(Bill(es, t))

                case Left(issues) =>
                    Left(CheckoutFailure(issues, items))
            }
        }

    }

    object Offers {

        private[this] def is(item: Item)(entry: Entry) = entry._1 == item
        private[this] def free(entry: Entry): Entry = (entry._1, op.zero)
        private[this] def discount(n: Int)(entries: List[Entry]): List[Entry] = {
            val (a, b) = entries.splitAt(entries.size - n)
            a ++ b.map(free)
        }

        def buyOneGetOneFree(item: Item): SpecialOffer =
            (entries: List[Entry], total: Price) => {
                val freeEntries = entries.filter(is(item)).map(free)
                (entries ++ freeEntries, total)
            }

        def nItemsForThePriceOfM(item: Item, n: Int, m: Int): SpecialOffer = {
            require(m > 0, "number of discounted items must be greater than 0")
            require(m < n, "item's group size must be larger than number of discounted items")
            (entries: List[Entry], total: Price) => {
                val (eligible, others) = entries.partition(is(item))
                val discounted = eligible.grouped(n).map(g => if (g.size < n) g else discount(n - m)(g)).flatten.toList
                val all = discounted ++ others
                (all, sumTotal(all))
            }
        }

    }
}

object SimpleShop extends Shop[String, BigDecimal]

