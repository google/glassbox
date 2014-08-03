# Visualization functions of glassbox

GenerateSingleSpaghettiPlot <- function(spans) {
  # TODO
}

GenerateSingleCategoricalPlot <- function(spans) {
  # TODO
}

CreateSpaghettiVis <- function(m,
                               test.df,
                               file,
                               col1 = rgb(1, 0, 0, 1),
                               col0 = rgb(0, 0, 0, 1),
                               point.cex = 1,
                               n.sample = min(nrow(test.df), 50),
                               loess.span = .80,
                               truncate.quantile.upper = .98,
                               truncate.quantile.lower = .02,
                               predictors = NULL) {
  # Creates "Spaghetti" visualization for random forest models to determine
  #   the true nature of the effects
  #
  # Args:
  # m: a fitted model (currently support random forests)
  # test.df: a holdout data set (perferably) structurally identical to
  #          m's training set
  # file: pdf file where the output is saved
  # col1: line color for the positive (TRUE) case
  # col2: line color for the negative (FALSE) case
  # point.cex: size of the points that will be plotted
  # n.sample: the number of lines to be plotted (sampled randomly if less than
  #           the number of rows)
  # loess.span: the amount of smoothing (from 0 to 1) to add to the individual
  #             predictions
  # truncate.quantile.upper: the quantile above which records are discarded
  # truncate.quantile.lower: the quantile below which records are discarded
  # predictors: vector of predictor names to use, if not all should be plotted
  #
  # Returns:
  #  Nothing. Pdf document saved in specified file location
  test.df <- as.data.frame(test.df)
  test.response <- test.df[, GetResponse(m)]
  pdf(file)
  sample.rows <- sample(seq(nrow(test.df)), n.sample)
  all.predictors <- GetPredictors(m)
  if (is.null(predictors)) predictors <- all.predictors
  k <- 0
  for (term in predictors) {
    k <- k + 1
    cat("\n", term, ", number ", k, "\n\n")
    term.obj <- test.df[, term]
    # Is the term graphable on a continuous scale?
    is.numeric.term <-
      (is.numeric(term.obj) | is.integer(term.obj)) & (var(term.obj) > 0)
    other.terms <- setdiff(all.predictors, term)
    if (is.numeric.term) {
      print(" is numeric.")  # Numeric plotting setup follows
      x.lower <- quantile(term.obj, truncate.quantile.lower)
      x.upper <- quantile(term.obj, truncate.quantile.upper)
      grid.df <-
        data.frame(term = seq(x.lower, x.upper, (x.upper - x.lower) / 20))
    } else {
      print(" is categorical")  # Categorical plotting setup follows
      grid.df <- data.frame(term = unique(term.obj))
    }
    names(grid.df) <- c(term)
    grid.df$pred <- -1
    grid.df <- grid.df[order(grid.df[, term]), ]
    plot.formula <- formula(paste("pred ~", term))
    print(plot.formula)
    plot(plot.formula, grid.df, type = "n",
         ylim = range(predict(m, test.df)),
         xlab = term, ylab = " Y label ",
         # TODO: find good Y label
         main = paste0(term, "-effect"))
    j <- 0
    pred.sum <- rep(0, nrow(grid.df))
    for (i in sample.rows) {
      j <- j + 1
      if (j %% 100 == 0) cat("sample row ", j, "\n")
      plot.col <- col1
      if (test.response[i] == "TRUE") plot.col <- col0
      # Fix other terms to a scalar value, taken by the i'th individual
      grid.df[, other.terms] <- test.df[i, other.terms]
      # TODO: generalize for binary response
      #      grid.df$pred <- predict(m, grid.df, type = "prob")[, "TRUE"]
      grid.df$pred <- Predict(m, grid.df)
      if (is.numeric.term) {
        try(row.loess <- loess(plot.formula, grid.df, span = loess.span,
                               degree = 1, family = "symmetric"))
        try(lines(row.loess$fitted ~ row.loess$x, col = plot.col))
        try(points(predict(row.loess, test.df[i, term]) ~
                   test.df[i, term], col = plot.col, pch = 17, cex = point.cex))
      } else {
        test.prediction <- Predict(m, test.df)
        lines(plot.formula, data=grid.df, col = plot.col)
        points(test.prediction[i] ~
               test.df[i, term], col = plot.col, pch = 17, cex = point.cex)
      }
      pred.sum <- pred.sum + grid.df$pred
    }
    pred.mean <- pred.sum / n.sample
    lines(pred.mean ~ grid.df[, term], col = "purple", lwd = 3)
  }
  dev.off()
}
