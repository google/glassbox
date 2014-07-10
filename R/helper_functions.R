# Copyright 2014 Google Inc. All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Functions that visualize relationships for black-box models

GetResponse <- function(m) {
  # Gets the character response from a fitted model
  #
  # Args:
  #   m: a fitted object (currently supports random forests only)
  #
  # Returns:
  #   String of response variable name
  #
  m.class <- class(m)
  if ("randomForest" %in% m.class) {
    model.formula <- eval(m$call$formula)
    response.var <- as.character(model.formula[[2]])
    response.var <- response.var[!(response.var %in% c("factor", "as.factor"))]
    return(response.var)
  }
  print("Unsupported model type")
}

GetPredictors <- function(m) {
  # Gets the predictors used to fit a fitted model
  #
  # Args:
  #   m: Fitted model object (currently supports random forests only)
  #
  # Returns:
  #   Character vector of predictors
  #
  m.class <- class(m)
  if ("randomForest" %in% m.class) {
    return(row.names(importance(m)))
  }
  print("Unsupported model type")
}

Predict <- function(m, newdata) {
  # Generic function to retrieve predictions of a model on new data
  #
  # Args:
  #   m: Fitted model  object (currently supports linear, logit, and random
  #      forests classification/regression
  #   newdata: Data to predict on
  #
  # Returns:
  #   Vector of predictions
  m.class <- class(m)
  if ("lm" %in% m.class) {
    if (family(m)$family == "binomial") { # Logit
      return(predict(m, newdata, type="response"))
    } else { # Linear regression
      return(predict(m, newdata))
    }
  } else if ("randomForest" %in% m.class) {
    if (m$type == "regression") {
      return(predict(m, newdata))
    } else {
      return(predict(m, newdata, type="prob")[, 2])
    }
  }
  print("Unsupported model type")
}

GenerateSpans <- function(x,
                          truncate.quantile.lower = 0.02,
                          truncate.quantile.upper = 0.98,
                          breakpoints = 20) {
  # Generates multiple rows of each record of dt with an individual variable
  # perturbed. Numeric variables are perturbed to values between quantiles,
  # and categorical variables are perturbed to each possible value.
  #
  # Args:
  #   x: Data frame or table with all observations from which to generate spans
  #
  # Returns:
  #   data.table object with all columns from x and multiple rows per
  #   observation (as described). A 'perturbed.column' field is also added to
  #   the result to indicate the column name that has been perturbed for that
  #   row.
  x <- data.table(x)
  # Initialize output
  y <- x[0]
  y[, perturbed.column := NA_character_]
  y[, is.perturbed.column.numeric := NA]
  # Stack each perturbed result
  # TODO: Pre-allocate
  for (col in names(x)) {
    y <- rbind(y, PerturbColumn(x, col))
  }
  return(y)
}

StackDataTable <- function(dt, n) {
  # Stacks a data.table object on top of itself multiple times
  #
  # Args:
  #   dt: Input data.table
  #   n: Number of times to stack
  #
  # Returns:
  #   data.table with dt stacked n times
  return(data.table(rbind.fill(replicate(n, dt, simplify=F))))
}

PerturbColumn <- function(x,
                          col,
                          truncate.quantile.lower = 0.02,
                          truncate.quantile.upper = 0.98,
                          breakpoints = 20) {
  is.numeric.col <- is.numeric(x[[col]])
  if (is.numeric.col) {
    y <- PerturbNumericColumn(x, col, truncate.quantile.lower,
                              truncate.quantile.upper, breakpoints)
  } else {
    y <- PerturbCategoricalColumn(x, col)
  }
  y[, perturbed.column := col]
  y[, is.perturbed.column.numeric := is.numeric.col]
  return(y)
}

PerturbNumericColumn <- function(x,
                                 col,
                                 truncate.quantile.lower = 0.02,
                                 truncate.quantile.upper = 0.98,
                                 breakpoints = 20) {
  # Replicates data.table multiple times, keeping all but one variable
  # constant while setting a variable of interest to a range of percentiles
  #
  # Args:
  #   x: data.table object to replicate
  #   col: Column name to perturb
  #   truncate.quantile.lower: the quantile below which records are discarded
  #   truncate.quantile.upper: the quantile above which records are discarded
  #   breakpoints: Number of breakpoints to build curve off
  col.v <- x[[col]]
  min.val <- quantile(col.v, truncate.quantile.lower, na.rm=T)
  max.val <- quantile(col.v, truncate.quantile.upper, na.rm=T)
  val.seq <- seq(min.val, max.val, (max.val - min.val) / breakpoints)
  y <- StackDataTable(x, length(val.seq))
  col.vals.expanded <- RepeatElements(val.seq, nrow(x))
  set(y, , col, col.vals.expanded)
  return(y)
}

PerturbCategoricalColumn <- function(x,
                                     col) {
  # Replicates data.table multiple times, keeping all but one variable
  # constant while setting a variable of interest to each of its possible values
  #
  # Args:
  #   x: data.table object to replicate
  #   col: Column name to perturb
  col.vals <- unique(x[[col]])
  y <- StackDataTable(x, length(col.vals))
  col.vals.expanded <- RepeatElements(col.vals, nrow(x))
  set(y, , col, col.vals.expanded) # check
  return(y)
}

RepeatElements <- function(v, n) {
  # Returns a vector which stacks each element of an input vector n times
  #
  # Args:
  #   v: vector
  #   n: number of times to repeat each element of v
  #
  # Returns:
  #   Vector with each element of vector v stacked n times
  #
  # Example:
  #   RepeatElements(seq(3), 3)
  #   # Returns c(1, 1, 1, 2, 2, 2, 3, 3, 3)
  return(as.vector(sapply(v, function(x) rep(x, n))))
}

CreateSpaghettiVisNew <- function(m,
                                  test.df,
                                  file,
                                  col1 = rgb(1, 0, 0, 1),
                                  col0 = rgb(0, 0, 0, 1),
                                  point.cex = 1,
                                  n.sample = min(nrow(test.df), 50),
                                  loess.span = .80,
                                  truncate.quantile.lower = 0.02,
                                  truncate.quantile.upper = 0.98,
                                  breakpoints = 20,
                                  predictors = NULL) {
  # Creates "spaghetti" visualization for random forest models to determine
  # the true nature of the effects
  #
  # Args:
  #   m: Fitted model object (currently support random forests)
  #   test.df: Holdout data set (preferably) structurally identical to m's
  #            training set
  #   file: PDF file where the output is saved
  #   col1: Line color for the positive (TRUE) case
  #   col2: Line color for the negative (FALSE) case
  #   point.cex: Size of the points that will be plotted
  #   n.sample: Number of lines to be plotted (sampled randomly if less than the
  #             number of rows)
  #   loess.span: Amount of smoothing (from 0 to 1) to add to the individual
  #               predictions
  #   truncate.quantile.lower: Quantile below which records are discarded
  #   truncate.quantile.upper: Quantile above which records are discarded
  #   breakpoints: Number of breakpoints to build curve off
  #   predictors: vector of predictor names to use, if not all should be plotted
  #
  # Returns:
  #   Nothing; PDF document saved in specified file location
  sample.rows <- sample(seq(nrow(test.df)), n.sample)
  test.dt <- data.table(test.df)[sample.rows]
  test.response <- test.dt[[GetResponse(m)]]
  pdf(file)
  all.predictors <- GetPredictors(m)
  if (is.null(predictors)) predictors <- all.predictors
  k <- 0
  spans <- GenerateSpans(test.dt)
  spans[, prediction := Predict(m, spans)]
  for (term in predictors) {
    temp <- spans[perturbed.column == term]
    if (temp[["is.perturbed.column.numeric"]][1]) {
      CreateSingleSpaghettiPlot(temp)
    } else {
      CreateSingleCategoricalPlot(temp)
    }
  }
  return(spans)
}
