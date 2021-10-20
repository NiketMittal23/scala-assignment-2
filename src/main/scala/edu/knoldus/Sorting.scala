package edu.knoldus

import scala.::

class Sorting {

  def insertionSort(array: Array[Int]): Array[Int] = {
    def insertionList(list: List[Int]): List[Int] ={
      def insert(number: Int, newList: List[Int]): List[Int] ={
        if (newList.isEmpty || number <= newList.head) number :: newList
        else newList.head :: insert(number, newList.tail)
      }
      if (list.isEmpty || list.tail.isEmpty) list
      else insert(list.head, insertionList(list.tail))
    }
    insertionList(array.toList).toArray
  }

  def selectionSort(array: Array[Int]): Array[Int] = {
    for(i <- 0 until array.length-1){
      var min = i
      for(j <- i + 1 until array.length){
        if(array(j) < array(min)){
          min = j
        }
        val temp = array(i)
        array(i) = array(min)
        array(min) = temp
      }
    }
    array
  }

  def bubbleSort(array: Array[Int]): Array[Int] = {
    def bubble(array: Array[Int], size: Int): Array[Int] = {
      for ( i <- 0 until size-1){
        for ( j <- 0 until size-i-1){
          if (array(j) > array(j+1)){
            val temp = array(j);
            array(j) = array(j+1);
            array(j+1) = temp;
          }
        }
      }
      array
    }
    bubble(array,array.length)
  }

}
