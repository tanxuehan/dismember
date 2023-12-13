package com.mass.tdm.evaluation

object Metrics {

  def computeMetrics(recItems: Array[Int], labels: Array[Int]): (Double, Double, Double) = {
    // 1 0 1 0 0 0 1
    // 0 1 2 3 4 5 6
    // dcg = a + a/log 2 + a/log 6 [0, 2, 6]
    // idcg = a + a + a/log 2 [0, 1, 2]
    val k = recItems.length
    val labelSet = labels.toSet
    var i, j, commonItems = 0
    var dcg, idcg = 0.0
    while (i < k) {
      if (labelSet.contains(recItems(i))) {
        commonItems += 1
        dcg += (java.lang.Math.log(2) / java.lang.Math.log(i + 2))
        idcg += (java.lang.Math.log(2) / java.lang.Math.log(j + 2))
        j += 1
      }
      i += 1
    }

    if (commonItems != 0) {
      // precision recall ndcg
      (commonItems.toDouble / k, commonItems.toDouble / labels.length, dcg / idcg)
    } else {
      (0.0, 0.0, 0.0)
    }
  }
}
