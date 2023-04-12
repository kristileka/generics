package kristileka.generics

import scala.annotation.tailrec

trait Set[A] extends (A => Boolean) {
  override def apply(value: A): Boolean = contains(value)

  def contains(value: A): Boolean

  def +(value: A): Set[A]

  def ++(setValue: Set[A]): Set[A]

  def map[B](f: A => B): Set[B]
}

class EmptySet[A] extends Set[A] {
  def contains(value: A): Boolean = false

  def +(value: A): Set[A] = new NonEmptySet[A](value, this)

  def ++(setValue: Set[A]): Set[A] = setValue

  def map[B](f: A => B): Set[B] = new EmptySet[B]
}

class NonEmptySet[A](head: A, tail: Set[A]) extends Set[A] {
  override def contains(value: A): Boolean = value == head || tail.contains(head)

  override def +(value: A): Set[A] = if (this.contains(value)) this else new NonEmptySet[A](value, this)

  override def ++(setValue: Set[A]): Set[A] = tail ++ setValue + head

  override def map[B](f: A => B): Set[B] = (tail map f) + f(head)
}
object Set {
  def apply[A](values: A*): Set[A] = {
    @tailrec
    def build(seq: Seq[A], acc: Set[A]): Set[A] =
      if (seq.isEmpty) acc
      else build(seq.tail, acc + seq.head)
    build(values, new EmptySet[A]())
  }
}

object Test extends App {
  var set = Set("This ","is " ,"a ","set")
  set.map(print(_))
}
