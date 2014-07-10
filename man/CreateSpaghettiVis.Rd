\name{CreateSpaghettiVis}
\alias{CreateSpaghettiVis}
\title{Create Spaghetti Visualizations}
\description{
	Creates "Spaghetti" visualization for random forest models to determine
    the true nature of the effectsCreate Spaghetti visualizations
}
\usage{
CreateSpaghettiVis(rf.model, test.df, file, col1 = rgb(1, 0, 0, 1),
            col0 = rgb(0, 0, 0, 1), point.cex = 1, n.sample = 50,
            loess.span = .80,
            truncate.quantile.upper = .98, truncate.quantile.lower = .02)
}
\arguments{
	\item{rf.model}{fitted random forest model}
	\item{test.df}{hold out data set}
	\item{file}{output location}
}

\examples{
  library(glassbox)
  data(testdata)
  library(randomForest)
  rf.model <- randomForest(y ~ x + z, data=testdata)
  CreateSpaghettiVis(rf.model, testdata, "~/Google Drive/filename.pdf",
  col1=rgb(0, 0, 0, .05), n.sample = 1000)

  # Binary case
  testdata$w <- runif(1000) <= plogis(.5 * testdata$x - .7 * testdata$z)
  train <- testdata[1:800, ]
  test <- testdata[801:1000, ]
  rf.model.2 <- randomForest(as.factor(w) ~ x + z, train)
  CreateSpaghettiVis(rf.model.2, test, "~/Google Drive/filename2.pdf",
  col0=rgb(1, 0, 0, .05), col1=rgb(0, 0, 0, 1), n.sample=200)
}
