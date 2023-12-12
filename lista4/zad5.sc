enum SolidFigure:
  case Cuboid(length: Double, width: Double, height: Double)
  case Cone(radius: Double, height: Double)
  case Sphere(radius: Double)
  case Cylinder(radius: Double, height: Double)

def volume(figure: SolidFigure): Double =
  figure match
    case SolidFigure.Cuboid(l, w, h) => l * w * h
    case SolidFigure.Cone(r, h) => Math.PI * r * r * h / 3
    case SolidFigure.Sphere(r) => 4 / 3 * Math.PI * r * r * r
    case SolidFigure.Cylinder(r, h) => Math.PI * r * r * h


val cuboid = SolidFigure.Cuboid(1, 2, 3)
val cone = SolidFigure.Cone(1, 2)
val sphere = SolidFigure.Sphere(1)
val cylinder = SolidFigure.Cylinder(1, 2)

volume(cuboid)
volume(cone)
volume(sphere)
volume(cylinder)


