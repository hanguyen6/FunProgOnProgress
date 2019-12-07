var a = 5
var b = a
println(a, b)
a = 6
println(a,b)

val a = Array(1,3,4)
val b = a
a(2) = 5
a.foreach(println)
b.foreach(println)