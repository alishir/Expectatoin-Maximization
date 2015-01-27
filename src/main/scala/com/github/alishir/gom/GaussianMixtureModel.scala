package com.github.alishir.gom

import breeze.linalg._
import breeze.numerics.sqrt
import breeze.plot._
import breeze.stats.distributions._
import BigInt._

/**
 * Created by ali on 1/24/15.
 */
object GaussianMixtureModel {
  val numGaussians = 2
  val dimension = 1
//  val sampleSize: BigInt = BigInt("10000000000000000")
    val sampleSize = 1000000

  def EM(data: DenseVector[Double]) = {
    // Initialization
    val portions = normalize(DenseVector.rand[Double](numGaussians), 1)
    val gaussians = DenseMatrix.rand[Double](numGaussians, 2) // first column in mean, second in variance

    for (i <- 0 to 100) {
      // Expectation Step
      val estimates: DenseVector[DenseVector[Double]] = data.map(datum => {
        val pdfs = gaussians(*, ::).map(g => Gaussian(g(0), g(1)).pdf(datum))
        if (!any(pdfs)) pdfs := DenseVector(0.00001, 0.00001)
        val nominator = portions :* pdfs
        val denominator = portions.t * pdfs
        nominator :/ denominator
      })

      // Maximization Step
      val partialSum = estimates.fold(DenseVector.zeros[Double](estimates(0).length))(_ :+ _)
      val total = sum(partialSum)
      portions := partialSum / total

      // Update means
      val weightedSum = estimates.mapPairs({
        case (index, estimate) => estimate * data(index)
      }).fold(DenseVector.zeros[Double](estimates(0).length))(_ :+ _)
      gaussians(::, 0) := (weightedSum :/ partialSum)

      // Update variances
      val weightedVarSum = estimates.mapPairs({
        case (index, estimate) => {
          val diff = gaussians(::, 0) - data(index)
          diff := (diff :* diff)
          estimate :* diff
        }
      }).fold(DenseVector.zeros[Double](estimates(0).length))(_ :+ _)
      gaussians(::, 1) := sqrt(weightedVarSum :/ partialSum)

      val f = new Figure("step-" + i, 1,1)
      val p1 = f.subplot(0)
      gaussians(*, ::).map(g => p1 += hist(Gaussian(g(0), g(1)).sample(10000), 1000))
      f.saveas("step-" + i +".png")
    }
    (portions, gaussians)
  }

  def main(args: Array[String]): Unit = {
    val (samples, gaussians, portions) = generateSamples()
    println(portions)
    draw(samples.toArray, gaussians.toArray, portions)
    val (emPortions, emGaussians) = EM(samples)
    println("Portions: " + emPortions)
    println("Gaussians: " + emGaussians)
  }

  def draw(samples: Array[Double], gaussians: Array[Gaussian], portions: DenseVector[Double]): Unit = {
    val f = new Figure("Real Gaussians and Data Histogram", 1,1)
    val p1 = f.subplot(0)
    val p2 = f.subplot(2,1,1)
    p1 += hist(samples, 1000)
    p1.title = "Samples histogram"
    val title = "Related Gaussians,\nweights are: (blue=%2.2f, orange=%2.2f)".format(portions(0), portions(1))
    p2.title = title
    for (gaussian <- gaussians) p2 += hist(gaussian.sample(10000), 1000)

    f.saveas("subplots.png")
  }

  def generateSamples() = {
    val portions = DenseVector.rand(numGaussians)
    val acc: DenseVector[Double] = accumulate(normalize(portions, 1))

    val meanDist = new Uniform(0, 50)
    val varianceDist = new Uniform(2,5)
    val gaussians = for (i <- 1 to numGaussians) yield new Gaussian(meanDist.draw(), varianceDist.draw())

    val uniform = new Uniform(0,1)
    val samples = for (i <- 0 until sampleSize) yield {
      val rand = uniform.draw()
      val gIndex = acc.toArray.zipWithIndex.filter(_._1 > rand)(0)._2
      val gaussian = gaussians(gIndex)
      gaussian.draw()
    }
    val ret = DenseVector.zeros[Double](samples.size)
    ret := new DenseVector[Double](samples.toArray)
    (ret, gaussians, normalize(portions, 1))
  }
}
