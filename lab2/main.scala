class StringMultiset private(private val elements: Map[String, Int]) {

  // Вспомогательный конструктор: принимает произвольное число строк и считает кратности, группируя по их значению,
  // вычисляется количество элементов (кратность) (view для ленивости вычисления)
  def this(strings: String*) = this(
    strings.groupBy(identity).view.mapValues(_.size).toMap
  )

  // Объединение: суммируются кратности элементов из обоих мультимножеств
  def +(other: StringMultiset): StringMultiset =
    new StringMultiset(
      (elements.keySet ++ other.elements.keySet)
        .map(key => key -> (elements.getOrElse(key, 0) + other.elements.getOrElse(key, 0)))
        .toMap
    )

  // Пересечение: для каждого элемента выбирается минимальное количество вхождений
  def *(other: StringMultiset): StringMultiset =
    new StringMultiset(
      (elements.keySet intersect other.elements.keySet)
        .map(key => key -> math.min(elements(key), other.elements(key)))
        .toMap
    )

  // Вычитание: из кратности элементов текущего мультимножества вычитаются кратности другого, и в результате остаются
  // только положительные значения
  def -(other: StringMultiset): StringMultiset =
    new StringMultiset(
      (elements.keySet ++ other.elements.keySet)
        .map(key => key -> (elements.getOrElse(key, 0) - other.elements.getOrElse(key, 0)))
        .filter { case (_, count) => count > 0 }
        .toMap
    )

  // Проверка на наличие строки в мультимножестве
  def contains(str: String): Boolean = elements.contains(str)

  // Получение кратности строки
  def count(str: String): Int = elements.getOrElse(str, 0)

  override def toString: String =
    elements.map { case (s, c) => s"$s -> $c" }.mkString("StringMultiset(", ", ", ")")
}

object Main {
  def main(args: Array[String]): Unit = {
    val ms1 = new StringMultiset("hello", "world", "hello")
    val ms2 = new StringMultiset("hello", "scala")

    println("ms1: " + ms1)
    println("ms2: " + ms2)

    val union = ms1 + ms2
    println("Union (ms1 + ms2): " + union)

    val intersection = ms1 * ms2
    println("Intersection (ms1 * ms2): " + intersection)

    val difference = ms1 - ms2
    println("Difference (ms1 - ms2): " + difference)

    println("Count (hello in ms1): " + ms1.count("hello"))

    println("Contains (halo in ms2): " + ms2.contains("halo"))
  }
}
