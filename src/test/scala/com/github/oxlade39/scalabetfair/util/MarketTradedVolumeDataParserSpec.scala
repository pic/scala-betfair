package com.github.oxlade39.scalabetfair.util

import org.specs2.mutable.Specification

/**
 * Created with IntelliJ IDEA.
 * User: pic
 * Date: 10/14/13
 * Time: 10:02 PM
 */
class MarketTradedVolumeDataParserSpec extends Specification {

  import MarketTradedVolumeDataParser._

  "inflatedMarketTradedVolume" should {

    "inflate market traded volume from compressed string representation" in {

      val res = inflateMarketTradedVolume(
        ":47998~0~0.0~0.0~0.0|10.0~0.04|10.5~16.31|13.0~104.07|13.5~18.7|14.0~33.02|14.5~4.98|15.0~2.35|15.5~2.35:1096~0~0.0~0.0~0.0|1.21~21.21|1.25~366.91|1.26~78.2|1.28~47.15|1.29~3109.5|1.3~5316.74|1.31~2136.78|1.32~2021.21:58805~0~0.0~0.0~0.0|5.8~236.84|5.9~122.28|6.0~191.43|6.2~78.77|6.6~36.25|6.8~43.98|9.0~0.15"
      )

      res.runnersInfo shouldEqual  List(
        RunnerInfo(47998, List(
          VolumeInfo(10.0,0.04),
          VolumeInfo(10.5,16.31),
          VolumeInfo(13.0,104.07),
          VolumeInfo(13.5,18.7),
          VolumeInfo(14.0,33.02),
          VolumeInfo(14.5,4.98),
          VolumeInfo(15.0,2.35),
          VolumeInfo(15.5,2.35))
        ),
        RunnerInfo(1096, List(
          VolumeInfo(1.21,21.21),
          VolumeInfo(1.25,366.91),
          VolumeInfo(1.26,78.2),
          VolumeInfo(1.28,47.15),
          VolumeInfo(1.29,3109.5),
          VolumeInfo(1.3,5316.74),
          VolumeInfo(1.31,2136.78),
          VolumeInfo(1.32,2021.21))
        ),
        RunnerInfo(58805, List(
          VolumeInfo(5.8,236.84),
          VolumeInfo(5.9,122.28),
          VolumeInfo(6.0,191.43),
          VolumeInfo(6.2,78.77),
          VolumeInfo(6.6,36.25),
          VolumeInfo(6.8,43.98),
          VolumeInfo(9.0,0.15))
        )
      )

      success
    }


  }

}
