package com.github.oxlade39.scalabetfair.response

import org.specs2.mutable.Specification
import com.betfair.publicapi.types.exchange.v5.{Market => BfMarket, Runner => BfRunner, _}
import io.Source
import org.joda.time.{DateTimeZone, DateTime}
import com.betfair.publicapi.types.global.v3.{EventType, ArrayOfEventType, GetEventTypesResp}
import com.github.oxlade39.scalabetfair.domain
import com.github.oxlade39.scalabetfair.domain._
import java.util.{GregorianCalendar, TimeZone}
import javax.xml.datatype.{DatatypeFactory, XMLGregorianCalendar}
import scala.Left
import com.github.oxlade39.scalabetfair.request.RequestError
import com.github.oxlade39.scalabetfair.domain.RunnerDetail
import com.github.oxlade39.scalabetfair.domain.Runner
import scala.Right
import scala.Some
import com.github.oxlade39.scalabetfair.domain.MarketName
import com.github.oxlade39.scalabetfair.request.Event
import com.github.oxlade39.scalabetfair.domain.MarketLiteDetail

/**
 * @author dan
 */
class ResponseParserSpec extends Specification {
  val londonTimezone = DateTimeZone.forID("Europe/London")

  val underTest = new RealResponseParserComponent {}.responseParser

  "ResponseParser" should {
    "return a request error if GetMarketResp has errors" in {

      val betfairResponse: GetMarketResp = new GetMarketResp()
      betfairResponse.setErrorCode(GetMarketErrorEnum.INVALID_MARKET)
      betfairResponse.setMinorErrorCode("no such market")

      val market = underTest.runnersFromMarket(betfairResponse)

      market mustEqual Right(RequestError("API error: %s:%s".format(GetMarketErrorEnum.INVALID_MARKET.value(), "no such market")))
    }

    "return a list of runners from GetMarketResp" in {

      val betfairResponse: GetMarketResp = new GetMarketResp()
      betfairResponse.setErrorCode(GetMarketErrorEnum.OK)
      val bfMarket = new BfMarket()
      val bfRunners = new ArrayOfRunner()
      bfRunners.getRunner.add(createRunner(1))
      bfRunners.getRunner.add(createRunner(2))
      bfMarket.setRunners(bfRunners)

      betfairResponse.setMarket(bfMarket)

      val market = underTest.runnersFromMarket(betfairResponse)

      market mustEqual Left(List(Runner("runner1", 1), Runner("runner2", 2)))
    }

    "return marketDetails from GetAllMarketsResp" in {
      val betfairResponse = new GetAllMarketsResp()

      betfairResponse.setMarketData(TestExamples.exampleMarketDataString)

      val response = underTest.toMarketDetails(betfairResponse)

      response.isLeft mustEqual true

      val details = response.left.get
      details.size mustEqual 106
      val firstMarketDetails = details.head

      firstMarketDetails.marketName mustEqual MarketName(107031725,"Both teams to Score?")
      firstMarketDetails.status mustEqual "ACTIVE"
      firstMarketDetails.amountMatched mustEqual 428.62
      firstMarketDetails.menuPath mustEqual List("Soccer","English Soccer","Npower League One","Fixtures 16 October","L Orient v Hartlepool")
      firstMarketDetails.betDelay mustEqual "8"
      firstMarketDetails.country mustEqual("GBR")

      val expectedEventDate = new DateTime(londonTimezone).withYear(2012).withMonthOfYear(10).withDayOfMonth(16)
        .withHourOfDay(19).withMinuteOfHour(45)
        .withSecondOfMinute(0).withMillisOfSecond(0)

      firstMarketDetails.eventDate mustEqual expectedEventDate
      firstMarketDetails.eventHierarchy mustEqual List("1", "258597", "1908054", "26905577", "26905578", "107031725")
      firstMarketDetails.exchangeId mustEqual 1
      firstMarketDetails.lastRefresh mustEqual expectedEventDate.withMinuteOfHour(57).withSecondOfMinute(25).withMillisOfSecond(292)
      firstMarketDetails.marketType mustEqual "O"
      firstMarketDetails.numPosWinners mustEqual 1
      firstMarketDetails.numRunners mustEqual 2
      firstMarketDetails.supportsStartingPrice mustEqual false
      firstMarketDetails.turningInPlay mustEqual false
    }

    "return marketDetails from GetAllMarketsResp bis (escape characters)" in {
      val betfairResponse = new GetAllMarketsResp()

      betfairResponse.setMarketData(TestExamples.exampleMarketDataStringBis)

      val response = underTest.toMarketDetails(betfairResponse)

      response.isLeft mustEqual true

      val details = response.left.get
      details.size mustEqual 178
      val firstMarketDetails = details.head

      firstMarketDetails.marketName mustEqual MarketName(111133750, "Man City Win from Behind")
      firstMarketDetails.status mustEqual "ACTIVE"
      firstMarketDetails.amountMatched mustEqual 0.0
      firstMarketDetails.menuPath mustEqual List("Soccer", "UEFA Champions League", "Fixtures 02 October", "Man City v B Munich")
      firstMarketDetails.betDelay mustEqual "0"
      firstMarketDetails.country mustEqual("GBR")

      val expectedEventDate = new DateTime(londonTimezone).withYear(2013).withMonthOfYear(10).withDayOfMonth(2)
        .withHourOfDay(19).withMinuteOfHour(45)
        .withSecondOfMinute(0).withMillisOfSecond(0)

      firstMarketDetails.eventDate mustEqual expectedEventDate
      firstMarketDetails.eventHierarchy mustEqual List("1", "78601", "27071149", "27071160", "111133750")
      firstMarketDetails.exchangeId mustEqual 1
      firstMarketDetails.lastRefresh mustEqual expectedEventDate.withDayOfMonth(1).withHourOfDay(22).withMinuteOfHour(49).withSecondOfMinute(32).withMillisOfSecond(153)
      firstMarketDetails.marketType mustEqual "O"
      firstMarketDetails.numPosWinners mustEqual 1
      firstMarketDetails.numRunners mustEqual 2
      firstMarketDetails.supportsStartingPrice mustEqual false
      firstMarketDetails.turningInPlay mustEqual false
    }

    "return a list of event from a GetEventTypesResp" in {

      val bfResponse = new GetEventTypesResp()
      val eventTypes = new ArrayOfEventType()
      eventTypes.getEventType.add(createEvent(45453, "Some Event"))
      eventTypes.getEventType.add(createEvent(45454, "Some Other Event"))
      bfResponse.setEventTypeItems(eventTypes)

      val response = underTest.toEvents(bfResponse)

      response.isLeft mustEqual true
      val events = response.left.get

      events mustEqual List(Event(45453, Some("Some Event")), Event(45454, Some("Some Other Event")))
    }

    "return MarketPrices from GetMarketPricesCompressedResp" in {

      val bfResponse = new GetMarketPricesCompressedResp
      bfResponse.setMarketPrices(TestExamples.exampleCompressedMarketPrices)

      val response: Either[domain.MarketPrices, RequestError] = underTest.toMarketPrices(bfResponse,
        MarketName(111179500, "Market Name"))

      response.isLeft mustEqual true
      val prices = response.left.get
      prices.market mustEqual MarketName(111179500, "Market Name")
      prices.inPlayDelay mustEqual 0
      prices.runners.size mustEqual 6

      val runnerDetail: RunnerDetail = prices.runners.head
      runnerDetail.runner mustEqual Runner("", 6307039)
      runnerDetail.totalAmountMatched mustEqual BigDecimal("4.0")
      runnerDetail.lastPriceMatched mustEqual BigDecimal("3.25")
      val bestBack = runnerDetail.bestBacks.head
      bestBack.backAvailable mustEqual BigDecimal("5.0")
      bestBack.price mustEqual BigDecimal("15.0")
    }

    "return MarketPrices from GetCompleteMarketPricesCompressedResp" in {

      val bfResponse = new GetCompleteMarketPricesCompressedResp
      bfResponse.setCompleteMarketPrices(TestExamples.exampleCompressedCompleteMarketPrices)

      val response: Either[domain.MarketPrices, RequestError] = underTest.toMarketPrices(bfResponse,
        MarketName(107119445, "Market Name"))

      response.isLeft mustEqual true
      val prices = response.left.get
      prices.market mustEqual MarketName(107119445, "Market Name")
      prices.inPlayDelay mustEqual 0
      prices.runners.size mustEqual 2

      val runnerDetail: RunnerDetail = prices.runners.sortBy {_.runner.selectionId}. head
      runnerDetail.runner mustEqual Runner("", 30246)
      runnerDetail.totalAmountMatched mustEqual BigDecimal("0.0")
      runnerDetail.lastPriceMatched mustEqual BigDecimal(0)
      val bestBack = runnerDetail.bestBacks.head
      bestBack.backAvailable mustEqual BigDecimal("123.86")
      bestBack.price mustEqual BigDecimal("1.02")
    }

    "return MarketLiteDetail from GetMarketInfoResp" in {
      val bfResponse = new GetMarketInfoResp()
      val marketLite = new MarketLite()

      val londonTimezone = DateTimeZone.forID("Europe/London")
      val time = new DateTime(londonTimezone).withMillis(DateTime.now.getMillis)
      val c: GregorianCalendar = new GregorianCalendar();
      c.setTimeInMillis(time.getMillis)

      marketLite.setDelay(6)
      marketLite.setMarketStatus(MarketStatusEnum.INACTIVE)
      marketLite.setMarketTime(DatatypeFactory.newInstance().newXMLGregorianCalendar(c))
      marketLite.setNumberOfRunners(7)
      bfResponse.setMarketLite(marketLite)
      bfResponse.setErrorCode(GetMarketErrorEnum.OK)

      val response = underTest.toMarketLiteDetail(bfResponse)

      response.isLeft mustEqual true
      val lite = response.left.get

      lite mustEqual MarketLiteDetail("INACTIVE", time, 6, 7)
    }

    "return InflatedMarketTraddVolume from GetMarketTradedVolumeCompressedResp" in {

      val bfResponse = new GetMarketTradedVolumeCompressedResp
      bfResponse.setTradedVolume(TestExamples.exampleCompressedMarketTradedVolume)

      val response: Either[MarketTradedVolume, RequestError] = underTest.toMarketTradedVolume(bfResponse)

      response.isLeft mustEqual true
      val volume = response.left.get.volume

      volume.runnersInfo.size mustEqual 3
      val rVolume = volume.runnersInfo(1)
      rVolume.selectionId shouldEqual 58943
      rVolume.tradedVolume should haveSize(11)

      var pVolume = rVolume.tradedVolume(4)
      pVolume.price shouldEqual BigDecimal("6.6")
      pVolume.tradedAmount shouldEqual BigDecimal("697.17")

      pVolume = rVolume.tradedVolume(5)
      pVolume.price shouldEqual BigDecimal("6.8")
      pVolume.tradedAmount shouldEqual BigDecimal("650.85")

    }
  }


  def createEvent(id: Int, eventName: String) = {
    val eventType = new EventType()
    eventType.setExchangeId(1)
    eventType.setId(id)
    eventType.setName(eventName)
    eventType
  }

  def createRunner(id: Int) = {
    val runner = new BfRunner()
    runner.setName("runner%s".format(id))
    runner.setSelectionId(id)
    runner
  }
}

object TestExamples {
  lazy val exampleMarketDataString = singleLineFromString("exampleMarketDataString.txt")
  lazy val exampleMarketDataStringBis = singleLineFromString("marketData.txt")
  lazy val exampleCompressedMarketPrices = singleLineFromString("compressedMarketPrices.txt")
  lazy val exampleCompressedCompleteMarketPrices = singleLineFromString("compressedCompleteMarketPrices.txt")
  lazy val exampleCompressedMarketTradedVolume = singleLineFromString("compressedMarketTradedVolume.txt")

  def singleLineFromString(fileName: String): String = {
    val resource = Thread.currentThread().getContextClassLoader.getResourceAsStream(fileName)
    val source = Source.fromInputStream(resource)
    try {
      source.getLines().next()
    } finally source.close()
  }

}
