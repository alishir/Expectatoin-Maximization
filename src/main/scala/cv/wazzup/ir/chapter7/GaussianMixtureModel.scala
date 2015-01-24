package cv.wazzup.ir.chapter7

import breeze.linalg._
import breeze.plot._
import breeze.stats.distributions._

/**
 * Created by ali on 1/24/15.
 */
object GaussianMixtureModel {
  val numGaussians = 2
  val dimension = 1
  val sampleSize = 10000

  def main(args: Array[String]): Unit = {
    val (samples, gaussians, portions) = generateSamples()
    println(portions)
    draw(samples.toArray, gaussians.toArray)
  }

  def draw(samples: Array[Double], gaussians: Array[Gaussian]): Unit = {

    val f = new Figure("salam", 1,1)
    val p1 = f.subplot(0)
    val p2 = f.subplot(2,1,1)
    p1 += hist(samples, 1000)
    for (gaussian <- gaussians) p2 += hist(gaussian.sample(10000), 1000)

    f.saveas("subplots.png")
  }

  def generateSamples() = {
    val portions = DenseVector.rand(numGaussians)
    val acc: DenseVector[Double] = accumulate(normalize(portions, 1))

    val meanDist = new Uniform(0, 20)
    val varianceDist = new Uniform(2,5)
    val gaussians = for (i <- 1 to numGaussians) yield new Gaussian(meanDist.draw(), varianceDist.draw())

    val uniform = new Uniform(0,1)
    val samples = for (i <- 0 to sampleSize) yield {
      val rand = uniform.draw()
      val gIndex = acc.toArray.zipWithIndex.filter(_._1 > rand)(0)._2
      val gaussian = gaussians(gIndex)
      gaussian.draw()
    }
    (samples, gaussians, normalize(portions, 1))
  }
}
