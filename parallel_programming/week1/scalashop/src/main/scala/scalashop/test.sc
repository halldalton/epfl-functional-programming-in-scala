import scalashop._

import scala.util.Random
val radius = 1
val width = 1920
val height = 1080
val numTasks = 32
val src = Img(width, height)
val dst = Img(width, height)

boxBlurKernel(src, 0, 0, 1)