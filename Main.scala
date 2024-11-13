object Main {
  def main(args: Array[String]): Unit = {
    val H_m = 65537
    val H_a = 75
    val seed = 2
    val n = 1000
    val amount_of_sailor = 1000


    def Homework_rand(seed:Int,a:Int,m:Int):LazyList[Long] = {
      seed#::Homework_rand((a*seed)%m,a,m)
    }
//
//    val answer:List[Long] = Homework_rand(seed,H_a,H_m).take(n).toList

    def NormalList(list:List[Long]):List[Double] = {
      val max = list.max()
      val min = list.min()
      list.map(x => Norma(x,min,max))
    }

    def Norma(x:Long, min:Long, max:Long):Double = {
      2 * (x - min).toDouble / (max - min) - 1
    }

    def To_calculate_sq_average(InputList:List[Double]):Double = {
      val temp = InputList.sum/InputList.length
      val sigma = Math.sqrt((InputList.map(x => (x - temp)*(x - temp)).sum)/InputList.length)
      sigma
    }
    def Homework_sailors(numbers_of_sailors:Int):Double = {
      val answer_static_steps: List[Double] = List()
      val temp_list: List[List[Double]] = (1 to numbers_of_sailors).toList.map(x => NormalList(Homework_rand(x, H_a, H_m).take(n).toList))
      val temp_list_2: List[Double] = temp_list.map(_.sum)
      val temp_list_3: List[Double] = temp_list_2.map(x => x / n)
      To_calculate_sq_average(temp_list_3)
    }

    def Homework_num(num_of_steps:Int):Double = {
      val answer_static_steps: List[Double] = List()
      val temp_list: List[List[Double]] = (1 to amount_of_sailor).toList.map(x => NormalList(Homework_rand(x, H_a, H_m).take(num_of_steps).toList))
      val temp_list_2: List[Double] = temp_list.map(_.sum)
      val temp_list_3: List[Double] = temp_list_2.map(x => x / num_of_steps)
      To_calculate_sq_average(temp_list_3)
    }

    val sigma_sailors:List[Double] = (1 to 1000).toList.map(x => Homework_sailors(x))
//    val sigma_num:List[Double] = (1 to 1000).toList.map(x => Homework_num(x))
    println(sigma_sailors)
//    println(sigma_num)

  }
}