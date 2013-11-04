package com.github.oxlade39.scalabetfair.response

import com.betfair.publicapi.types.exchange.v5._
import com.github.oxlade39.scalabetfair.domain._
import com.betfair.publicapi.types.global.v3.GetEventTypesResp
import org.joda.time.{DateTimeZone, DateTime}
import scala.Left
import com.github.oxlade39.scalabetfair.request.RequestError
import com.github.oxlade39.scalabetfair.domain.MarketPrices
import com.github.oxlade39.scalabetfair.request.Event
import com.github.oxlade39.scalabetfair.domain.Runner
import com.github.oxlade39.scalabetfair.domain.MarketDetail
import scala.Right
import scala.Some
import com.betfair.publicapi.util.InflatedCompleteMarketPrices.{InflatedCompletePrice, InflatedCompleteRunner}
import com.betfair.publicapi.util.InflatedCompleteMarketPrices
import com.github.oxlade39.scalabetfair.domain.RunnerPrice
import com.github.oxlade39.scalabetfair.util.MarketPricesDataParser
import com.github.oxlade39.scalabetfair.util.MarketTradedVolumeDataParser
import com.github.oxlade39.scalabetfair.util.MarketTradedVolumeDataParser.InflatedMarketTradedVolume

/**
 * @author dan
 */
trait ResponseParserComponent {

  def responseParser: ResponseParser

  trait ResponseParser {
    def toEvents(response: GetEventTypesResp): Either[List[Event], RequestError]
    def toMarketLiteDetail(response: GetMarketInfoResp): Either[MarketLiteDetail, RequestError]
    def toMarketDetails(response: GetAllMarketsResp): Either[List[MarketDetail], RequestError]
    def runnersFromMarket(response: GetMarketResp): Either[List[Runner], RequestError]
    def runnerPrice(response: InflatedCompletePrice): RunnerPrice
    def toMarketPrices(response: GetMarketPricesCompressedResp, marketName: MarketName): Either[MarketPrices, RequestError]
    def toMarketPrices(response: GetCompleteMarketPricesCompressedResp,
                       marketName: MarketName): Either[MarketPrices, RequestError]
    def toCompleteMarketPricesData(response: GetCompleteMarketPricesCompressedResp, timestamp: Long): Either[CompleteMarketPricesData, RequestError]

    def toMarketTradedVolume(response: GetMarketTradedVolumeCompressedResp): Either[MarketTradedVolume, RequestError]
    def toMarketTradedVolumeData(response: GetMarketTradedVolumeCompressedResp, timestamp: Long): Either[MarketTradedVolumeData, RequestError]

  }
}

trait RealResponseParserComponent extends ResponseParserComponent {
  import scala.collection.JavaConversions._

  import com.github.oxlade39.scalabetfair.util.ParseToolkit.escapedSplit

  val responseParser = new ResponseParser {

    def toMarketPrices(response: GetMarketPricesCompressedResp,
                       marketName: MarketName): Either[MarketPrices, RequestError] = {

      import MarketPricesDataParser._

      val prices: InflatedMarketPrices = inflateMarketPrices(response.getMarketPrices)

      assert(prices.marketData.marketId.equals (marketName.id),
        "The market name must match the market in the compressed prices. %s != %s".format(prices.marketData.marketId, marketName.id))

      val runnerDetails: List[RunnerDetail] = prices.runnersInfo.map { runnerInfo: RunnerInfo =>
        val runnerPrices: List[RunnerPrice] = (runnerInfo.backPrices ++ runnerInfo.layPrices).map { p: PriceInfo =>
          RunnerPrice(
            p.price,
            p.layAvailable,
            p.backAvailable
          )
        }

        RunnerDetail(Runner("", runnerInfo.selectionId),
          runnerInfo.lastPriceMatched.getOrElse(null.asInstanceOf[BigDecimal]),
          runnerInfo.totalAmountMatched,
          runnerPrices
        )
      }
      val removedRunners = prices.marketData.removedRunners
      Left(MarketPrices(marketName, prices.marketData.delay, removedRunners, runnerDetails, Some(response.getMarketPrices)))
    }

    def toMarketPrices(response: GetCompleteMarketPricesCompressedResp,
                       marketName: MarketName): Either[MarketPrices, RequestError] = {

      val prices: InflatedCompleteMarketPrices = new InflatedCompleteMarketPrices(response.getCompleteMarketPrices)

      assert(prices.getMarketId.equals (marketName.id),
        "The marketname must match the market in the compressed prices. %s != %s".format(prices.getMarketId, marketName.id))

      val runnerDetails: List[RunnerDetail] = prices.getRunners.map {
        case (bfRunner: InflatedCompleteRunner) =>
          RunnerDetail(Runner("", bfRunner.getSelectionId),
            bfRunner.getLastPriceMatched,
            bfRunner.getTotalAmountMatched,
            bfRunner.getPrices.map(price => runnerPrice(price)).toList)
      }.toList
      val removedRunners: List[RemovedRunnerInfo] = prices.getRemovedRunners.toList.map { removedRunner =>
        RemovedRunnerInfo(removedRunner.getName, removedRunner.getRemovedDate, removedRunner.getAdjustmentFactor)
      }
      Left(MarketPrices(marketName, prices.getInPlayDelay, removedRunners, runnerDetails, Some(response.getCompleteMarketPrices)))
    }

    def runnersFromMarket(response: GetMarketResp): Either[List[Runner], RequestError] = {
      response.getErrorCode match {
        case GetMarketErrorEnum.OK => {
          val jmarket: Market = response.getMarket
          val jrunnersArray: ArrayOfRunner = jmarket.getRunners
          val jrunners = jrunnersArray.getRunner
          val runnerBuffer = jrunners.map(bfRunner => Runner(bfRunner.getName, bfRunner.getSelectionId))

          Left(runnerBuffer.toList)
        }
        case _ => Right(RequestError("API error: %s:%s".format(response.getErrorCode, response.getMinorErrorCode)))
      }
    }

    def toCompleteMarketPricesData(response: GetCompleteMarketPricesCompressedResp, timestamp: Long): Either[CompleteMarketPricesData, RequestError] = {
      Left(CompleteMarketPricesData(timestamp, Option(response.getCompleteMarketPrices)))
    }

    def toMarketLiteDetail(response: GetMarketInfoResp) =
      response.getErrorCode match {
        case GetMarketErrorEnum.OK => {
          val jmarket: MarketLite = response.getMarketLite

          Left(MarketLiteDetail(
            jmarket.getMarketStatus.value(),
            new DateTime(londonTimezone).withMillis(jmarket.getMarketTime.toGregorianCalendar.getTimeInMillis), //eventDate: DateTime
            jmarket.getDelay, // betDelay: String
            jmarket.getNumberOfRunners
          ))
        }
        case _ => Right(RequestError("API error: %s:%s".format(response.getErrorCode, response.getMinorErrorCode)))

      }

    def toMarketDetails(response: GetAllMarketsResp) =
      Option(response).flatMap(r => Option(r.getMarketData)).map(parseGetAllMarketsRespString(_)) match {
        case None => Right(RequestError("API error: %s:%s".format(response.getErrorCode, response.getMinorErrorCode)))
        case Some(x) => Left(x)
      }

    def toEvents(response: GetEventTypesResp): Either[List[Event], RequestError] = {
      import scala.collection.JavaConversions._

      val eventTypes = Option(response.getEventTypeItems).flatMap(et => Option(et.getEventType))
      eventTypes match {
        case None => Right(RequestError("API error: %s:%s".format(response.getErrorCode, response.getMinorErrorCode)))
        case Some(events) => Left(events.toList.map(eventType => Event(eventType.getId, Some(eventType.getName))))
      }
    }

    def runnerPrice(response: InflatedCompletePrice): RunnerPrice =
      RunnerPrice(
        response.getPrice,
        response.getBackAmountAvailable,
        response.getLayAmountAvailable
      )

    def toMarketTradedVolume(response: GetMarketTradedVolumeCompressedResp): Either[MarketTradedVolume, RequestError] = {

      import MarketTradedVolumeDataParser._

      val volume: Option[InflatedMarketTradedVolume] = Option(response.getTradedVolume).map { inflateMarketTradedVolume(_) }

      Left(MarketTradedVolume(response.getMarketId, response.getCurrencyCode, volume))
    }

    def toMarketTradedVolumeData(response: GetMarketTradedVolumeCompressedResp, timestamp: Long): Either[MarketTradedVolumeData, RequestError] = {
      Left(MarketTradedVolumeData(response.getMarketId, response.getCurrencyCode, timestamp, Option(response.getTradedVolume)))
    }

  }

  val londonTimezone = DateTimeZone.forID("Europe/London")

  private[this] def parseGetAllMarketsRespString(responseString: String): List[MarketDetail] = {
    val marketsUnparsed: List[String] = escapedSplit(responseString, ":")
    marketsUnparsed.toList.filter(!_.isEmpty) map {
      singleMarketUnparsed: String =>
        val marketFields: Array[String] = singleMarketUnparsed.split("~")
        val marketDetail: MarketDetail = new MarketDetail(
          MarketName(marketFields(0).toInt, marketFields(1)),
          marketFields(2),
          marketFields(3),
          new DateTime(londonTimezone).withMillis(marketFields(4).toLong),
          marketFields(5).split("\\\\").drop(1).toList.map(_.trim()),
          marketFields(6).split("/").drop(1).toList,
          marketFields(7),
          marketFields(8).toInt,
          marketFields(9),
          new DateTime(londonTimezone).withMillis(marketFields(10).toLong),
          marketFields(11).toInt,
          marketFields(12).toInt,
          marketFields(13).toDouble,
          marketFields(14).eq("Y"),
          marketFields(15).eq("Y")
        )
        marketDetail
    }
  }
}

