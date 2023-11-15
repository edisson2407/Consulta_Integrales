//Definimos la funcion f(x) como una funcion lambda
val f = (x : Double) => - math.pow(x, 2) + 8*x - 12

//Creamos la funcion con nommbre que recibe como parametros una funcion y dos enteros
// y esta realiza la operacion usando el metodo de integracion denominado Simpson 1/3

def integracion(f: Double => Double, a: Int, b: Int): Double = {
  val X = (a + b) / 2
  (b-a) * (f(a) + 4*f(X) + f(b)) / 6
}
//se define otra funcion para realizar el calculo de margen de error del valor absoluto entre
// el valor esperado menos la aproximacion
def mError (y:Double, z:Double) : Double = math.abs(y-z)

//enviamos la funcion y los valores de a y b que son los limites de las integrales definidas
  val aprox = integracion(f,3,5)

//aqui se envia lo que es el valor esperado y la respuesta de  la integral definida
  val margenError = mError(7.33, aprox )

//------------------------------------------------------------------------------//

val f2 = (x : Double) => 3 * math.pow(x, 2)


val aprox2 = integracion(f2,0,2)

val margenError2 = mError(8.00, aprox2 )

//------------------------------------------------------------------------------//

val f3 = (x : Double) => x + 2 * math.pow(x, 2) - math.pow(x, 3) + 5 * math.pow(x, 4)


val aprox3 = integracion(f3,-1,1)

val margenError3 = mError(3.333, aprox3 )


//------------------------------------------------------------------------------//

val f4 = (x : Double) => (2*x + 1) / math.pow(x,2) + x


val aprox4 = integracion(f4,1,2)

val margenError4 = mError(1.09861, aprox4 )


//------------------------------------------------------------------------------//

val f5 = (x : Double) => math.pow(math.E, x)


val aprox5 = integracion(f5,0,1)

val margenError5 = mError(1.71828, aprox5 )

//------------------------------------------------------------------------------//

val f6 = (x : Double) => 1 / math.sqrt(x-1)


val aprox6 = integracion(f6,2,3)

val margenError6 = mError(0.828427, aprox6)

//------------------------------------------------------------------------------//

val f7 = (x : Double) => 1 / 1 + math.pow(x,2)


val aprox7 = integracion(f7,0,1)

val margenError7 = mError(0.785398, aprox7)