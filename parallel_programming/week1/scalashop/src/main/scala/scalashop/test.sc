import scalashop._

val radius = 1
val width = 1920
val height = 1080
val src = Img(width, height)
val dst = Img(width, height)

boxBlurKernel(src, 3, 3, radius)