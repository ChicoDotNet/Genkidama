import kotlin.math.PI
import kotlin.math.abs
object VisitorExample{sealed interface Shape{data class Circle(val r:Double):Shape;data class Rect(val w:Double,val h:Double):Shape};fun area(s:Shape)=when(s){is Shape.Circle->PI*s.r*s.r;is Shape.Rect->s.w*s.h};fun run()=abs(listOf<Shape>(Shape.Circle(2.0),Shape.Rect(3.0,4.0)).sumOf(::area)-(4*PI+12))<1e-9}
