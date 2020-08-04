#' CV Survey function
#'
#' This function combines the other cross validation functions in this package
#' into a single function that is able to interpret datasets and also
#' survey design objects and survey glms
#' @export



cv.svy.glm <- function(Data, formulae, nfolds=5, N, clusterID, strataID, method = "linear", sample_type = "SRS",
                       design_object = NULL, glm = NULL) {

  if (is.null(glm) == FALSE) {
    model = glm
    b <- paste0(model[["formula"]][[2]])
    c <- paste0(model[["formula"]][[3]])

    if (length(b) == 2) {

      b1 <- b[1]
      b2 <- b[2]
      ys <- paste0(b1, "(", b2, ")")

      if (length(c) == 3) {
        c1 <- c[1]
        c2 <- c[2]
        c3 <- c[3]

        if (c1 == "ns") {
          xs <- paste0(c1, "(", c2, ",df=", c3, ")")
        } else if (c1 == "poly") {
          xs <- paste0(c1, "(", c2, ",raw=", c3, ")")
        } else {xs <- paste0(c2, c1, c3)}

      } else if (length(c) == 2) {
        c1 <- c[1]
        c2 <- c[2]
        xs <- paste0(c1, "(", c2, ")")
      } else if (length(c) == 1) {
        xs <- c
      }

      formulae = paste0(ys, "~", xs)

    } else if (length(b) == 1) {

      if (length(c) == 3) {
        c1 <- c[1]
        c2 <- c[2]
        c3 <- c[3]

        if (c1 == "ns") {
          xs <- paste0(c1, "(", c2, ",df=", c3, ")")
        } else if (c1 == "poly") {
          xs <- paste0(c1, "(", c2, ",raw=", c3, ")")
        } else {xs <- paste0(c2, c1, c3)}

      } else if (length(c) == 2) {
        c1 <- c[1]
        c2 <- c[2]
        xs <- paste0(c1, "(", c2, ")")
      } else if (length(c) == 1) {
        xs <- c
      }

      formulae = paste0(model[["formula"]][[2]], "~", xs)

    }

    design_object = model[["survey.design"]]

  }

  if (is.null(design_object) == FALSE) {
    Data = cbind(design_object[["variables"]], design_object[["cluster"]], design_object[["strata"]])
    cluster_var = gsub("~", "", paste0(design_object[["call"]][["ids"]]))

    if (cluster_var[2] != "0" & cluster_var[2] != "1") {
      sample_type = "Cluster"
      clusterID = cluster_var[2]
    }

    if (design_object[["has.strata"]] == TRUE) {
      sample_type = "Strat"
      strataID = paste0(design_object[["call"]][["strata"]][[2]])
    }

  }

  if (sample_type == "SRS") {
    cv.srs.lm(Data, formulae, nfolds = nfolds, N = N, method = method)
  } else if (sample_type == "Cluster") {
    cv.cluster.lm(Data, formulae, nfolds = nfolds, clusterID = clusterID, N = N, method = method)
  } else if (sample_type == "Strat") {
    cv.strat.lm(Data, formulae, nfolds = nfolds, strataID = strataID, N = N, method = method)
  }

}

