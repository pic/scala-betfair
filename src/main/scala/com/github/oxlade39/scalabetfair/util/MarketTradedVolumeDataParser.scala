package com.github.oxlade39.scalabetfair.util

/**
 * Created with IntelliJ IDEA.
 * User: pic
 * Date: 10/10/13
 * Time: 9:51 PM
 */
object MarketTradedVolumeDataParser {

  import ParseToolkit._

  class InvalidMarketTradedVolumeData extends RuntimeException
  object InvalidMarketTradedVolumeData extends InvalidMarketTradedVolumeData

  case class VolumeInfo(val price: BigDecimal, val tradedAmount: BigDecimal) {
    require { price > 1.0 && price <= 1000.0}
    require { tradedAmount >= 0.0 }
  }

  case class RunnerInfo(
                        val selectionId: Int,
                        val tradedVolume: List[VolumeInfo]
                       )

  case class InflatedMarketTradedVolume(val sourceData: String, val runnersInfo: List[RunnerInfo])

  def  inflateMarketTradedVolume(data: String): InflatedMarketTradedVolume = {

    val parts = escapedSplit(data, ":")

    if (parts.isEmpty)
      throw InvalidMarketTradedVolumeData
    else {

      //val parts = escapedSplit(data, ":")
      val parts = data.split(":").dropWhile { _.isEmpty }
      val runnersInfo = parts.map { data: String => extractRunnerData(data) }.toList
      InflatedMarketTradedVolume(data, runnersInfo)
    }
  }

  private def extractRunnerData(data: String) = {
    //val mainParts = escapedSplit(data, "|")
    val mainParts = data.split("""\|""")
    //val firstPart = escapedSplit(mainParts.head, "~")
    val firstParts = mainParts.head.split("~")
    val runnerVolume = mainParts.slice(1, mainParts.length).map { runnerData: String =>
      val volumeParts = runnerData.split("~")
      VolumeInfo(toBigDecimal(volumeParts(0)).get, toBigDecimal(volumeParts(1)).get)
    }
    RunnerInfo(firstParts.head.toInt, runnerVolume.toList)
  }
}
