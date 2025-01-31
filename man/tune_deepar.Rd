% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tune_deepar.R
\name{tune_deepar}
\alias{tune_deepar}
\title{Function to tune DeepAR}
\usage{
tune_deepar(
  id,
  freq,
  recipe,
  horizon,
  splits,
  length,
  cv_slice_limit,
  most_important = TRUE,
  assess = "12 weeks",
  skip = "4 weeks",
  initial = "12 months",
  multiple_gpu = FALSE,
  no_gpu,
  min_obs_cv_train = 1,
  epochs = NULL,
  lookback = NULL,
  batch_size = NULL,
  learn_rate = NULL,
  num_cells = NULL,
  num_layers = NULL,
  scale = NULL,
  dropout = NULL
)
}
\arguments{
\item{id}{A quoted column name that tracks the GluonTS FieldName "item_id"}

\item{freq}{A pandas timeseries frequency such as "5min" for 5-minutes or "D" for daily.}

\item{recipe}{A gluonts recipe}

\item{horizon}{The forecast horizon}

\item{length}{The number of distinct hyperparameter for each tunable parameter}

\item{cv_slice_limit}{How many slice/folds in the tsCV}

\item{assess}{The number of samples used for each assessment resample}

\item{skip}{A integer indicating how many (if any) additional resamples to skip to thin the total amount of data points in the analysis resample.}

\item{initial}{The number of samples used for analysis/modeling in the initial resample.}

\item{multiple_gpu}{Should more than one GPU be used}

\item{no_gpu}{How many, if more than one, should be used}

\item{min_obs_cv_train}{Minimum observation in the training set during cross validation}

\item{epochs}{Number of epochs. Importance 1 of 7}

\item{lookback}{Lookback length. If NULL, will be randomly chosen. Importance 2 of 7}

\item{batch_size}{batch_size Number of examples in each batch. Importance 3 of 7}

\item{learn_rate}{Learning rate. Importance 4 of 7}

\item{num_cells}{Number of RNN cells for each layer. Importance 5 of 7}

\item{num_layers}{Number of RNN layers. No info on importance}

\item{scale}{Scales numeric data by id group using mean = 0, standard deviation = 1 transformation. No info on importance}
}
\description{
Function to tune DeepAR
}
