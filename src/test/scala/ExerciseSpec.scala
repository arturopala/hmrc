package exercise

import org.scalatest.{WordSpecLike, Matchers, Inside}
import org.scalatest.prop.PropertyChecks
import org.scalacheck._

class SomeSpec extends WordSpecLike with Matchers with PropertyChecks with Inside {
    import SimpleShop._

    implicit override val generatorDrivenConfig = PropertyCheckConfig(minSize = 0, maxSize = 100, minSuccessful = 100, workers = 5)

    val Apple = "Apple"
    val Orange = "Orange"
    val Banana = "Banana"

    val appleListGen: Gen[List[String]] = Gen.listOf(Gen.const(Apple))
    val orangeListGen: Gen[List[String]] = Gen.listOf(Gen.const(Orange))
    val bananaListGen: Gen[List[String]] = Gen.listOf(Gen.const(Banana))
    val otherListGen: Gen[List[String]] = Gen.listOf(Gen.alphaStr)

    val pricelist = Map[String, BigDecimal](Apple -> 0.60, Orange -> 0.25)

    "SimpleShop" must {

        val cashdesk = new Cashdesk(pricelist.get)

        "sum total price of items" in {
            forAll(appleListGen, orangeListGen) {
                (apples, oranges) =>
                    val entries = (apples ++ oranges).map(i => (i, pricelist(i)))
                    val total = cashdesk.sumTotal(entries)
                    total shouldBe (apples.size * pricelist(Apple) + oranges.size * pricelist(Orange))
            }
        }

        "checkout empty list of items" in {
            val items = List()
            inside(cashdesk.checkout(items)) {
                case Right(Bill(entries, total)) =>
                    total shouldBe 0
                    entries should have size (0)
                case Left(CheckoutFailure(issues, items)) => fail
            }
        }

        "checkout list of known items" in {
            val items = List(Apple, Orange)
            inside(cashdesk.checkout(items)) {
                case Right(Bill(entries, total)) =>
                    total shouldBe 0.85
                    entries should have size (2)
                case Left(CheckoutFailure(issues, items)) => fail
            }
        }

        "not checkout list containing unknown item" in {
            val items = List(Apple, Orange, Banana)
            inside(cashdesk.checkout(items)) {
                case Left(CheckoutFailure(issues, items)) =>
                    issues shouldBe Set(Banana)
                    items should have size (3)
                case Right(Bill(entries, total)) => fail
            }
        }

        "checkout various lists of known items" in {
            forAll(appleListGen, orangeListGen) {
                (apples, oranges) =>
                    val items = apples ++ oranges
                    inside(cashdesk.checkout(items)) {
                        case Right(Bill(entries, total)) =>
                            total shouldBe (apples.size * pricelist(Apple) + oranges.size * pricelist(Orange))
                            entries should have size (apples.size + oranges.size)
                        case Left(CheckoutFailure(issues, items)) => fail
                    }
            }
        }

        "not checkout various lists containing unknown items" in {
            forAll(appleListGen, orangeListGen, bananaListGen, otherListGen) {
                (apples, oranges, bananas, others) =>
                    whenever(bananas.size > 0 && others.size > 0) {
                        val items = apples ++ oranges ++ bananas ++ others
                        inside(cashdesk.checkout(items)) {
                            case Left(CheckoutFailure(issues, items)) =>
                                issues should contain(Banana)
                                for (other <- others) issues should contain(other)
                                items should have size (apples.size + oranges.size + bananas.size + others.size)
                            case Right(Bill(entries, total)) => fail
                        }
                    }
            }
        }

    }

    "Offers.buyOneGetOneFree" must {

        "add free apple for one existing" in {
            val entries: List[Entry] = List((Apple, 0.60))
            Offers.buyOneGetOneFree(Apple)(entries) shouldBe List((Apple, 0.60), (Apple, 0))
        }

        "add free apple for each one existing" in {
            val entries: List[Entry] = List((Apple, 0.60), (Apple, 0.60))
            Offers.buyOneGetOneFree(Apple)(entries) shouldBe List((Apple, 0.60), (Apple, 0.60), (Apple, 0), (Apple, 0))
        }

        "add free apple for each one existing but not for orange" in {
            val entries: List[Entry] = List((Apple, 0.60), (Orange, 0.25), (Apple, 0.60))
            Offers.buyOneGetOneFree(Apple)(entries) shouldBe List((Apple, 0.60), (Orange, 0.25), (Apple, 0.60), (Apple, 0), (Apple, 0))
        }

        "return unmodified if don't contain apples" in {
            val entries: List[Entry] = List((Orange, 0.25))
            Offers.buyOneGetOneFree(Apple)(entries) shouldBe List((Orange, 0.25))
        }
    }

    "Offers.nItemsForThePriceOfM" must {

        "not discount when number of items less then threshold" in {
            val entries: List[Entry] = List((Orange, 0.25), (Orange, 0.25))
            Offers.nItemsForThePriceOfM(Orange, 3, 2)(entries) shouldBe List((Orange, 0.25), (Orange, 0.25))
        }

        "discount third orange and recalculate price" in {
            val entries: List[Entry] = List((Orange, 0.25), (Orange, 0.25), (Orange, 0.25))
            Offers.nItemsForThePriceOfM(Orange, 3, 2)(entries) shouldBe List((Orange, 0.25), (Orange, 0.25), (Orange, 0))
        }

        "discount each third orange" in {
            val entries: List[Entry] = List((Orange, 0.25), (Orange, 0.25), (Orange, 0.25), (Orange, 0.25), (Orange, 0.25))
            Offers.nItemsForThePriceOfM(Orange, 3, 2)(entries) shouldBe List((Orange, 0.25), (Orange, 0.25), (Orange, 0), (Orange, 0.25), (Orange, 0.25))
        }

        "not change number of entries and recalculate price" in {
            forAll(appleListGen, orangeListGen) {
                (apples, oranges) =>
                    val entries = (apples ++ oranges).map(i => (i, pricelist(i)))
                    val modified = Offers.nItemsForThePriceOfM(Orange, 3, 2)(entries)
                    modified.size shouldBe entries.size
            }
        }
    }

}
