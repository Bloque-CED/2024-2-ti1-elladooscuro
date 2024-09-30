package co.edu.icesi.u

import scala.annotation.tailrec

/**
 * Clase `HeapSort` que implementa el algoritmo de ordenamiento por montículos (Heap Sort) de manera recursiva.
 * Utiliza una estructura de listas para representar los montículos y permite ordenar listas de enteros.
 */
class HeapSort {

  /**
   * Método privado `removeMax` que elimina y devuelve el elemento máximo de un montículo.
   *
   * @param heap La lista de enteros que representa el montículo.
   * @return Una opción que contiene una tupla con el elemento máximo y el montículo actualizado, o `None` si el montículo está vacío.
   */
  private def removeMax(heap: List[Int]): Option[(Int, List[Int])] = heap match {
    case Nil => None // Si el montículo está vacío, devolver None.
    case _ :: Nil =>
      val maxElement = heap.head // Si solo hay un elemento, devolverlo como el máximo.
      Some((maxElement, Nil)) // El nuevo montículo será una lista vacía.
    case head :: tail =>
      val maxElement = heap.head // El primer elemento es el máximo.
      // Crear un nuevo montículo ajustado después de eliminar el máximo.
      val newHeap = heapifyDown(heap.last :: tail.init, 0, heap.length - 1)
      Some((maxElement, newHeap))
  }

  /**
   * Método `sort` que ordena una lista de enteros utilizando el algoritmo de Heap Sort.
   *
   * @param heap La lista de enteros a ordenar.
   * @return La lista ordenada en orden ascendente.
   */
  def sort(heap: List[Int]): List[Int] = {
    // Función auxiliar recursiva para ir eliminando el máximo y acumulando los elementos ordenados.
    def sortAcc(heap: List[Int], sorted: List[Int]): List[Int] = {
      removeMax(heapifyDown(heap, 0, heap.length)) match {
        case Some((maxElement, newHeap)) => sortAcc(newHeap, maxElement :: sorted)
        case None => sorted
      }
    }
    // Revertir la lista resultante para obtener el orden correcto.
    reverse(sortAcc(heap, Nil))
  }

  /**
   * Método auxiliar `reverse` que invierte una lista de manera recursiva.
   *
   * @param list La lista a invertir.
   * @tparam T El tipo de los elementos en la lista.
   * @return La lista invertida.
   */
  private def reverse[T](list: List[T]): List[T] = {
    @tailrec
    def reverseHelper(remaining: List[T], reversed: List[T]): List[T] = {
      remaining match {
        case Nil => reversed
        case head :: tail => reverseHelper(tail, head :: reversed)
      }
    }
    reverseHelper(list, Nil)
  }

  /**
   * Método auxiliar `heapifyDown` que ajusta el montículo después de eliminar el máximo para restaurar la propiedad de heap.
   *
   * @param list La lista de enteros que representa el montículo.
   * @param idx El índice desde el cual se comienza el ajuste.
   * @param heapSize El tamaño actual del montículo.
   * @return La lista de enteros ajustada como un montículo.
   */
  @tailrec
  private def heapifyDown(list: List[Int], idx: Int, heapSize: Int): List[Int] = {
    // Funciones para calcular los índices de los hijos izquierdo y derecho.
    def leftChild(i: Int): Int = 2 * i + 1
    def rightChild(i: Int): Int = 2 * i + 2

    (list, idx) match {
      case (Nil, _) => list // Si la lista está vacía, devolverla.
      case (_, i) if i >= heapSize => list // Si el índice está fuera del tamaño del montículo, devolver la lista.
      case (lst, i) =>
        val left = leftChild(i)
        val right = rightChild(i)

        // Determinar el índice del mayor entre el nodo actual y sus hijos.
        val largest = (if (left < heapSize && lst(left) < lst(i)) left else i) match {
          case tmp if right < heapSize && lst(right) < lst(tmp) => right
          case tmp => tmp
        }

        if (largest != i) {
          // Intercambiar el elemento actual con el mayor de sus hijos y continuar ajustando.
          val swapped = swap(lst, i, largest)
          heapifyDown(swapped, largest, heapSize)
        } else {
          lst
        }
    }
  }

  /**
   * Método auxiliar `swap` que intercambia dos elementos en una lista.
   *
   * @param list La lista de enteros.
   * @param i El índice del primer elemento.
   * @param j El índice del segundo elemento.
   * @return Una nueva lista con los elementos en los índices `i` y `j` intercambiados.
   */
  private def swap(list: List[Int], i: Int, j: Int): List[Int] = {
    (list, i, j) match {
      case (Nil, _, _) => list // Si la lista está vacía, devolverla tal cual.
      case (_, x, y) if x == y => list // Si los índices son iguales, no hacer nada.
      case (lst, x, y) =>
        val (min, max) = if (x < y) (x, y) else (y, x)
        val (beforeMin, afterMin) = lst.splitAt(min)
        val (minElement, afterMax) = afterMin.splitAt(1)
        val (middle, maxElement :: rest) = afterMax.splitAt(max - min - 1)
        beforeMin ::: maxElement :: middle ::: minElement ::: rest
    }
  }

}
