import scala.io.Source
import java.io.PrintWriter
import java.util.Date

case class Order(
    orderId: Int,
    userName: String,
    orderTime: Long,
    orderType: String,
    quantity: Int,
    price: Int
)

case class Match(
    orderId1: Int,
    orderId2: Int,
    price: Int,
    quantity: Int,
    matchTime: Long
)

object MatchingEngine {
    def main(args: Array[String]): Unit = {
        val orders = readOrders("exampleOrders.csv")
        val orderBook = scala.collection.mutable.Map.empty[String, List[Order]]
        val matches = scala.collection.mutable.ListBuffer.empty[Match]

        for (order <- orders) {
            if (order.orderType == "BUY") {
                val sellOrders = orderBook.getOrElse("SELL", List.empty)
                val bestPriceOrder = findBestPriceOrder(sellOrders, order.price, order.quantity)

                if (bestPriceOrder.isDefined) {
                    // Matched!
                    val matchedOrder = bestPriceOrder.get
                    matches += Match(order.orderId, matchedOrder.orderId, matchedOrder.price, order.quantity, System.currentTimeMillis())
                    orderBook("SELL") = removeMatchedOrder(sellOrders, matchedOrder)
                } else {
                    // No match, add to the buy orders in the order book
                    orderBook("BUY") = orderBook.getOrElse("BUY", List.empty) :+ order
                }
            } else if (order.orderType == "SELL") {
                val buyOrders = orderBook.getOrElse("BUY", List.empty)
                val bestPriceOrder = findBestPriceOrder(buyOrders, order.price, order.quantity)

                if (bestPriceOrder.isDefined) {
                    // Matched!
                    val matchedOrder = bestPriceOrder.get
                    matches += Match(order.orderId, matchedOrder.orderId, matchedOrder.price, order.quantity, System.currentTimeMillis())
                    orderBook("BUY") = removeMatchedOrder(buyOrders, matchedOrder)
                } else {
                    // No match, add to the sell orders in the order book
                    orderBook("SELL") = orderBook.getOrElse("SELL", List.empty) :+ order
                }
            }
        }

        // Write matches to a CSV file
        writeMatches("outputExampleMatches.csv", matches.toList)
    }

    def readOrders(filename: String): List[Order] = {
        val bufferedSource = Source.fromFile(filename)
        val orders = scala.collection.mutable.ListBuffer.empty[Order]
        for (line <- bufferedSource.getLines.drop(1)) {
            val cols = line.split(",").map(_.trim)
            if (cols.length == 6) {
                val order = Order(cols(0).toInt, cols(1), cols(2).toLong, cols(3), cols(4).toInt, cols(5).toInt)
                orders += order
            }
        }
        bufferedSource.close()
        orders.toList
    }
