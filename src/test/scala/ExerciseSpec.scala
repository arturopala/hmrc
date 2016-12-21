package exercise

import org.scalatest.{WordSpecLike, Matchers, Inside}
import org.scalatest.prop.PropertyChecks
import org.scalacheck._

class SomeSpec extends WordSpecLike with Matchers with PropertyChecks with Inside {

    implicit override val generatorDrivenConfig = PropertyCheckConfig(minSize = 1, maxSize = 100, minSuccessful = 100, workers = 5)

    val appleListGen: Gen[List[String]] = Gen.listOf(Gen.const("Apple"))
    val orangeListGen: Gen[List[String]] = Gen.listOf(Gen.const("Orange"))
    val bananaListGen: Gen[List[String]] = Gen.listOf(Gen.const("Banana"))
    val otherListGen: Gen[List[String]] = Gen.listOf(Gen.alphaStr)

    "AppleOrangeShop" must {

        import AppleOrangeShop._

        val pricelist = Map[String, BigDecimal]("Apple" -> 0.60, "Orange" -> 0.25)
        val cashdesk = new Cashdesk(pricelist.get)

        "checkout empty list of items" in {
            val items = List()
            inside(cashdesk.checkout(items)) {
                case Right(Bill(entries, total)) =>
                    total shouldBe 0
                    entries should have size (0)
            }
        }

        "checkout list of known items" in {
            val items = List("Apple", "Orange")
            inside(cashdesk.checkout(items)) {
                case Right(Bill(entries, total)) =>
                    total shouldBe 0.85
                    entries should have size (2)
            }
        }

        "not checkout list containing unknown item" in {
            val items = List("Apple", "Orange", "Banana")
            inside(cashdesk.checkout(items)) {
                case Left(CheckoutFailure(issues, entries)) =>
                    issues shouldBe Set("Banana")
                    entries should have size (3)
            }
        }

        "checkout various lists of known items" in {
            forAll(appleListGen, orangeListGen) {
                (apples, oranges) =>
                    val items = apples ++ oranges
                    inside(cashdesk.checkout(items)) {
                        case Right(Bill(entries, total)) =>
                            total shouldBe (apples.size * pricelist("Apple") + oranges.size * (pricelist("Orange")))
                            entries should have size (apples.size + oranges.size)
                    }
            }
        }

        "not checkout various lists containing unknown items" in {
            forAll(appleListGen, orangeListGen, bananaListGen, otherListGen) {
                (apples, oranges, bananas, others) =>
                    whenever(bananas.size > 0 && others.size > 0) {
                        val items = apples ++ oranges ++ bananas ++ others
                        inside(cashdesk.checkout(items)) {
                            case Left(CheckoutFailure(issues, entries)) =>
                                issues should contain("Banana")
                                for (other <- others) issues should contain(other)
                                entries should have size (apples.size + oranges.size + bananas.size + others.size)
                        }
                    }
            }
        }

    }

}
