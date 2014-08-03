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

# Functions that evaluate model effectiveness and variable importance

VarShare <- function(rf.obj, members) {
  # Calculates share of splits in the forest involving predictors
  # See http://stats.stackexchange.com/q/92419/19676
  count <- table(rf.obj$forest$bestvar)[-1]
  names(count) <- names(rf.obj$forest$ncat)
  share <- count[members] / sum(count[members])
  return(share)
}

GroupImportance <- function(rf.obj, groups) {
  # Calculates importance of groupings of predictors in a random forests model
  # Adapted from http://stats.stackexchange.com/q/92419/19676
  #
  # Args:
  #   rf.obj: Random forests model object
  #   groups: List of variable grouping vectors, e.g.
  #           list(Sepal=c("Sepal.Width", "Sepal.Length"),
  #                Petal=c("Petal.Width", "Petal.Length"))
  #
  # Returns:
  #   Matrix o
  var.imp <- as.matrix(sapply(groups, function(g) {
    sum(importance(rf.obj, 2)[g, ] * VarShare(rf.obj, g))
  }))
  colnames(var.imp) <- "MeanDecreaseGini"
  return(var.imp)
}

BrierScore <- function(observed, pred) {
  # Calculates Brier score, a MSE analog for binomial responses
  #
  # Args:
  #   observed: Vector of 1/0 or T/F indicating observed values
  #   pred: Vector of predicted probabilities
  #
  # Returns:
  #   Number indicating model's Brier score
  return(mean((pred - observed) ^ 2, na.rm=T))
}
