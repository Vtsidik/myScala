import java.io.File
import java.io.PrintWriter
import scala.io.Source.fromFile

class EllipticCurve(pin: Int, ain: Int, bin: Int){
  var p: Int = pin
  var a: Int = ain
  var b: Int = bin
  val O = new Point2D(0,0)
  var PointVec = new Array[Point2D](1)
  PointVec(0) = O

  def getAllPoints()
  {
    var x_1 = new Array[Double](p)
    var x_2 = new Array[Double](p)
    var y_1 = new Array[Double](p)
    var y_2 = new Array[Double](p)

    for ( i <- 0 to p-1)  {
      x_1(i) = i
      y_1(i) = i
      x_2(i) = (cube(i) + a * i + b) % p
      y_2(i) = kva(i) % p
    }

    var count = 0

    for (j <- 0 to y_2.length-1){
      for(i <- 0 to x_2.length-1){
        if (y_2(j) == x_2(i)){
          count += 1
        }
      }
    }

    PointVec = new Array[Point2D](count+1)
    PointVec(0) = O
    var can = 1
      for (j <- 0 to y_2.length-1){
      for(i <- 0 to x_2.length-1){
        if (y_2(j) == x_2(i)){
          var pt = new Point2D(x_1(i), y_1(j))
          PointVec(can) = pt
          can += 1
        }
      }
    }

    return PointVec
  }

  def add(X: Point2D,Y: Point2D): Point2D ={
    var s,x,y: Double = 0.0
    var S = new Point2D(0,0)

    val xX = X.getX()
    val yX = X.getY()
    val xY = Y.getX()
    val yY = Y.getY()

    if (X == O){
      return Y
    } else if (Y == O) return X

    val oby = -Y.getY

    if (X.getX == Y.getX && X.getY == oby){
      return O
    }

    if (X.getX != Y.getX){
      s = ((yY - yX)/(xY - xX)) % p
      x = ( kva(s) - xX - xY) % p
      y = (-yX + s * (xX - x)) % p
      S = new Point2D(x, -y)
      return S
    }

    if (X.getX == Y.getX && X.getY == Y.getY && X.getY != 0){
      s = ((3 * kva(xX) + a)/(2 * yX)) % p
      x = (kva(s) - 2 * xX) % p
      y = (-yX + s * (xX - x)) % p
      S = new Point2D(x, -y)
      return S
    }

    return O
  }

  def getOrder(X: Point2D): Int = {
    var a: Int = 1
    var count: Int = 0
    var Temp = new Point2D(X.getX,X.getY)

    for(i <- 0 to p-1) {
      if (Temp == O) {
        return a
      } else {
        Temp = add(Temp, Temp)
        count = count + 1
      }
    }
    a = count
    return a
  }

  def cube(x: Double): Double = x * x * x
  def kva(x: Double): Double = x * x
}

class Point2D(xc: Double, yc: Double){
  var x: Double = xc
  var y: Double = yc

  def getX(): Double = x
  def getY(): Double = y

  override def toString: String =
    s"($x, $y)"
}

object Run {
  val line = fromFile("C:\\Users\\по\\Desktop\\EllipticCurve\\src\\main\\scala\\param.txt").mkString
  val paramArray = line.split("\\s+")
  var i = 1

  def main(args: Array[String]): Unit =
  {

    val filePath = "C:\\Users\\по\\Desktop\\EllipticCurve\\src\\main\\scala\\otvet.txt"
    val writer = new PrintWriter(new File(filePath))
    println(line)
    val param = paramArray(0) + " " + paramArray(1)+ " " + paramArray(2) + "\n"
    writer.write(param)
    val EllipCurve = new EllipticCurve(paramArray(0).toInt,paramArray(1).toInt,paramArray(2).toInt)
    EllipCurve.getAllPoints()

    //var order = EllipCurve.getOrder(EllipCurve.PointVec(1))
    //println(order)

    for(i <-0 to EllipCurve.PointVec.length-1){
      var Temp = new Point2D(0,0)
      Temp = EllipCurve.PointVec(i)

      if(Temp == EllipCurve.O){
        writer.write("O 1 \n")
      } else {
        var order = EllipCurve.getOrder(Temp)
        var str = "(" + EllipCurve.PointVec(i).getX.toInt + "," + EllipCurve.PointVec(i).getY.toInt + ") " + order +" \n"
        writer.write(str)
      }
    }

    writer.close()
  }
}


