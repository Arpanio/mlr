#' @export
makeRLearner.cluster.Cluster_Medoids = function() {
  makeRLearnerCluster(
    cl = "cluster.Cluster_Medoids",
    package = "ClusterR",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "clusters", default = 2L, lower = 1L),
      makeDiscreteLearnerParam(id = "distance_metric", values = c("euclidean", "manhattan", "chebyshev", "canberra", "braycurtis", "pearson_correlation", "simple_matching_coefficient", "minkowski", "hamming", "jaccard_coefficient", "Rao_coefficient", "mahalanobis", "cosine"), default = "euclidean"),
      makeNumericLearnerParam(id = "minkowski_p", default = 1L, lower = 0L),
      makeIntegerLearnerParam(id = "threads", default = 1L, lower = 1L, tunable = FALSE),
      makeLogicalLearnerParam(id = "swap_phase", default = TRUE),
      makeLogicalLearnerParam(id = "fuzzy", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "verbose", default = FALSE, tunable = FALSE),
      makeIntegerLearnerParam(id = "seed", default = 1L, lower = 0L, tunable = FALSE)
    ),
    par.vals = list(clusters = 2L, distance_metric = "euclidean", minkowski_p = 1L, threads = 1L, swap_phase = TRUE, fuzzy = FALSE, verbose = FALSE, seed = 1L),
    properties = c("numerics", "prob"),
    name = "Cluster_Medoids",
    note = "Calls Cluster_Medoids of package ClusterR. Argument clusters has default value of 2 if not specified by user.",
    short.name = "clust_medoid",
    callees = c("Cluster_Medoids", "predict_Medoids")
  )
}

#' @export
trainLearner.cluster.Cluster_Medoids = function(.learner, .task, .subset, .weights = NULL, ...) {
  ClusterR::Cluster_Medoids(getTaskData(.task, .subset), ...)
}

#' @export
predictLearner.cluster.Cluster_Medoids = function(.learner, .model, .newdata, ...) {
  if (.learner$predict.type == "prob") {
    pred = ClusterR::predict_Medoids(data = .newdata,
      MEDOIDS = .model$learner.model$medoids,
      fuzzy = TRUE, ...)

    res = pred$fuzzy_clusters

    return(res)
  } else {
    pred = ClusterR::predict_Medoids(data = .newdata,
      MEDOIDS = .model$learner.model$medoids,
      fuzzy = FALSE, ...)

    res = as.integer(pred$clusters)

    return(res)
  }
}
