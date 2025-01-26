# markovchain

Esta práctica forma parte de la asignatura Álgebra Lineal del Grado de Ciencia de Datos Aplicada.
Puede contener errores, por lo que no me hago responsable en caso de copia. Se ha subido únicamente con el objetivo de crear un proceso de mejora y práctica de los diferentes ejercicios.

A continuación se describe el contexto y preguntas de la práctica:

## Cadenas de Markov discretas

Una tienda en línea desea analizar el comportamiento de sus clientes para optimizar la experiencia de usuario e incrementar las ventas. La actividad de un cliente se puede modelar como un proceso de Markov, con los siguientes estados:

Entrada al sitio web (E): El cliente comienza la visita. Este estado representa el momento en el que un cliente accede al sitio web, ya sea desde un enlace directo, una campaña publicitaria o mediante una búsqueda. Es el punto inicial para todos los clientes.

Visualización de productos (V): El cliente navega por las páginas de los productos. En este estado, el cliente está explorando diferentes productos o categorías dentro del sitio web. Esta etapa incluye acciones como leer descripciones, ver imágenes, consultar reseñas y comparar opciones.

Añadir un producto al carrito (A): El cliente selecciona un producto para la compra. Este estado indica que el cliente ha seleccionado uno o más productos para la compra y los ha añadido al carrito de compra virtual. Este paso muestra una clara intención de compra, pero aún no es definitivo.

Proceder al pago (P): El cliente inicia el proceso de pago. En este estado, el cliente ha decidido avanzar hacia el proceso de pago. Aquí se pueden introducir datos de facturación, información de envío y seleccionar opciones de pago. Este paso es clave, ya que muchos clientes pueden abandonar el sitio en este punto (por ejemplo, si encuentran costes de envío inesperados).

Finalización de la compra (F): El cliente completa la transacción. Este estado es el objetivo principal del modelo: el cliente ha completado con éxito la compra. Esto implica que se han confirmado los detalles del pago y el pedido está registrado.

Salida del sitio web sin compra (S): El cliente abandona el sitio web. Este estado representa el momento en el que el cliente abandona el sitio web sin completar ninguna compra. Esto puede ocurrir en cualquier momento del proceso, ya sea por pérdida de interés, insatisfacción con los productos, dificultades técnicas u otros motivos.

### Cuadro 1: Matriz de transición entre el estado actual y el siguiente estado

| Estado actual | E     | V           | A     | P     | F     | S     |
|---------------|-------|-------------|-------|-------|-------|-------|
| **E**         | 0.10  | 0.50        |       |       |       | 0.40  |
| **V**         |       | 3(1 - α)/5  | α     |       |       | 2(1 - α)/5 |
| **A**         |       | 0.20        | 0.60  |       |       | 0.20  |
| **P**         |       |             |       | 0.10  | 0.80  | 0.10  |
| **F**         |       |             |       |       | 1.00  |       |
| **S**         |       |             |       |       |       | 1.00  |

***Valor de α: 0.55***

### Pregunta 1

Considerar la matriz de transición de la Tabla 1 y definir la matriz de transición 
P por filas.
Comprobar que la suma de las probabilidades de cada una de las seis filas es 1. Calcular también la suma de las probabilidades de cada una de las columnas.

### Pregunta 2

Identificar los estados absorbentes.

### Pregunta 3

Si un cliente comienza en el estado de entrada (E), ¿cuál es la probabilidad de que finalice una compra (F) después de 2 pasos? ¿Y después de 3 pasos?

### Pregunta 4

El código simulate_path —que se presenta a continuación— es una función que simula el recorrido de un cliente por la tienda en línea según una cadena de Markov definida por la matriz de transición P y donde la variable steps es un entero positivo que especifica el número de pasos que se quieren simular.
La salida de la función es un vector path de longitud steps que representa los estados visitados. Tener en cuenta que el estado 1 corresponde a E el estado 2 corresponde a V, y así sucesivamente.

```r
simulate_path <- function (P , steps ) {
  current_state <- 1
  path <- numeric ( steps )
  for ( i in 1: steps ) {
    path [ i ] <- current_state
    current_state <- sample (1: nrow ( P ) , size = 1 , prob = P [ current _ state , ])
  }
  return ( path )
}
```

Calcular el porcentaje de clientes que llegan al estado 
F o 5.

### Pregunta 5

Calcular los estados estacionarios del sistema (vectores propios asociados al valor propio 1).

### Pregunta 6

Cuando se trabaja con cadenas de Markov con estados absorbentes, como es el caso en esta práctica, a menudo es conveniente reorganizar la matriz de manera que las filas y columnas correspondientes a los estados absorbentes se enumeren primero.
Esto se denomina forma canónica. Reescribir la matriz de transición de la Tabla 1 en la forma canónica. Obtendréis una matriz de transición con esta estructura:

$$
Q =
\begin{bmatrix}
I_\kappa & 0_{\kappa \times (n - \kappa)} \\
A & D
\end{bmatrix}
$$

Donde:

- \(n = 6\) es el número de estados de la cadena de Markov.
- \(\kappa\) es el número de estados absorbentes.
- \(I_\kappa\) es la matriz identidad de \(\kappa\) filas y \(\kappa\) columnas.
- \(0_{\kappa \times (n - \kappa)}\) es la matriz nula de \(\kappa\) filas y \((n - \kappa)\) columnas.
- \(A\) es una matriz de \((n - \kappa)\) filas y \(\kappa\) columnas.
- \(D\) es una matriz de \((n - \kappa)\) filas y \((n - \kappa)\) columnas.

### Pregunta 7

Para determinar la tendencia a largo plazo usamos la matriz solución calculada como:

$$
S = \lim_{n \to \infty} Q^n
$$

En el caso de una cadena de Markov con estados absorbentes, se demuestra que:

$$
S =
\begin{bmatrix}
I_\kappa & 0_{\kappa \times (n - \kappa)} \\
S_r & 0_\kappa
\end{bmatrix}
$$

Donde:

$$
S_r = (I_{n - \kappa} - D)^{-1} A
$$

- \( S_r \) es la **matriz solución reducida**, que excluye las columnas asociadas a los estados no absorbentes y las filas asociadas a los estados absorbentes.
- \( I_{n-\kappa} \) es la matriz identidad de dimensión \( n-\kappa \).
- \( A \) y \( D \) están definidas en la forma canónica de la matriz.
- \( S_r \) tiene \( (n-\kappa) \) filas (correspondientes a los estados no absorbentes) y \( \kappa \) columnas (correspondientes a los estados absorbentes).

Calcular, con R, la matriz Sr.

### Pregunta 8

¿Qué proporción de clientes, a largo plazo, acabará comprando un producto? ¿Qué proporción saldrá sin comprar nada?
