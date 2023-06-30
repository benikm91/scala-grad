package scalagrad.showcase.deeplearning

import java.util.concurrent.TimeUnit

object Util:

    def time[R](block: => R): R = {
        val t0 = System.nanoTime()
        val result = block    // call-by-name
        val t1 = System.nanoTime()
        val ds = TimeUnit.SECONDS.convert(t1 - t0, TimeUnit.NANOSECONDS)
        println("Elapsed time: " + (ds) + "s")
        result
    }

    def timeMeasure[R](block: => R, unit: TimeUnit = TimeUnit.SECONDS): (R, Double) = {
        val t0 = System.nanoTime()
        val result = block    // call-by-name
        val t1 = System.nanoTime()
        val ds = unit.convert(t1 - t0, TimeUnit.NANOSECONDS)
        (result, ds)
    }
