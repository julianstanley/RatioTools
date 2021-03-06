% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SensorClass.R
\name{SensorPoint}
\alias{SensorPoint}
\title{An additional function-constructor for seperate x and y arrays}
\usage{
SensorPoint(xOx, yOx, xRed, yRed, E0)
}
\arguments{
\item{xOx}{A numeric array representing wavelength 
values in an oxidized state}

\item{yOx}{A numeric array representing relative absorption 
at the wavelengths of xOx}

\item{xRed}{A numeric array representing wavelength 
values in a reduced state}

\item{yRed}{A numeric array representing relative absorption
at the wavelenghts of xRed}

\item{E0}{a numeric value for the midpoint potential of the sensor}
}
\value{
A sensor object
}
\description{
An additional function-constructor for seperate x and y arrays
}
