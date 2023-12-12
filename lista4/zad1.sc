
type Point2D = (Double, Double)

def distance(p1: Point2D, p2: Point2D): Double =
  val dx = p1(0) - p2(0)
  val dy = p1(1) - p2(1)
  Math.sqrt(dx * dx + dy * dy)


val p1: Point2D = (2,2)
val p2: Point2D = (2,5)

distance(p1, p2)


type PointND = List[Double]

def distanceN(p1: PointND, p2: PointND): Double =
  def help(p1: PointND, p2: PointND): Double =
    (p1, p2) match
      case (Nil, Nil) => 0
      case (h1 :: t1, h2 :: t2) => (h1 - h2) * (h1 - h2) + help(t1, t2)
      case _ => throw new IllegalArgumentException("Points must have the same number of dimensions")

  Math.sqrt(help(p1,p2))


val pN1: PointND = List(1,2,3,4)
val pN2: PointND =  List(4,5,6)

distanceN(pN1, pN2)
