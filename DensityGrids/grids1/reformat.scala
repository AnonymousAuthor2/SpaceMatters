
import io._

val src = Source.fromFile(args(0)).getLines

println(src.next)

src.foreach { line =>
  val column = line.split(",")
  println((column(0).toDouble.toInt - 1) + "," + (column(1).toDouble.toInt - 1) + "," + column(2))
}

