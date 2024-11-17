object Main {
  def main(args: Array[String]): Unit = {
    val H_m = 65537
    val H_a = 75
    val max_step = 5000
    def rand(seed:Int,a:Int,m:Int):LazyList[Long] = {
      seed#::rand((a*seed)%m,a,m)
    }
    def Norma(x:Long, min:Long, max:Long):Double = {
      2 * (x - min).toDouble / (max - min) - 1
    }
    def NormalList(list: List[Long]): List[Double] = {
      val max = list.max()
      val min = list.min()
      list.map(x => Norma(x, min, max))
    }
    def To_calculate_sq_average(InputList:List[Double]):Double = {
      Math.sqrt((InputList.map(x=> Math.pow(InputList.sum/InputList.length - x,2)).sum)/InputList.length)
    }
    def Sq_average_steps_for_list_of_sailors_without_division(numbers_of_sailors: Int, numbers_of_steps: Int): Double = {
      To_calculate_sq_average((1 to numbers_of_sailors).toList.map(x => NormalList(rand(x + numbers_of_steps, H_a, H_m).take(numbers_of_steps).toList)).map(_.sum))
    }
    println((1 to max_step).toList.map(x => Sq_average_steps_for_list_of_sailors_without_division(1000,x)))
  }
}