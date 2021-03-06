% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wpgpCovariates.R
\name{wpgpGetZonalStats}
\alias{wpgpGetZonalStats}
\title{wpgpGetZonalStats function will download a ZonalStats csv}
\usage{
wpgpGetZonalStats(ISO3 = NULL, covariate = NULL, stat = "mean",
  destDir = tempdir(), username = NULL, password = NULL, quiet = TRUE,
  overwrite = TRUE, frCSVDownload = FALSE, method = "auto")
}
\arguments{
\item{ISO3}{a 3-character country code}

\item{covariate}{Covariate name.}

\item{stat}{Either as character: 'mean', 'min', 'max', 'sum'.}

\item{destDir}{Path to the folder where you want to save ZonalStats file}

\item{username}{ftp username to WorldPop ftp server}

\item{password}{ftp password to WorldPop ftp server}

\item{quiet}{Download Without any messages if TRUE}

\item{overwrite}{Logical. Overwrite the ZonalStats csv file if it already exists}

\item{frCSVDownload}{If TRUE, a new wpgAllCovariates.csv file will downloaded}

\item{method}{Method to be used for downloading files. Current download methods
are "internal", "wininet" (Windows only) "libcurl", "wget" and
"curl", and there is a value "auto"}
}
\value{
dataframe
}
\description{
wpgpGetZonalStats function will download a ZonalStats csv
}
\examples{
wpgpGetZonalStats("AGO","ccilc_dst011_2000", stat = "mean" ,"G:/WorldPop/",username="ftpUsername",password="ftpPassword")
}
