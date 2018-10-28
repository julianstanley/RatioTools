#' An S4 class to represent a sensor
#' @slot ox A 2xN numeric matrix, 
#' representing [x, y] of sensor in oxidized state
#' @slot red A 2xN numeric matrix, 
#' representing [x, y] of sensor in reduced state
#' @slot E0 a numeric value for the midpoint potential of the sensor
#' @importFrom methods new
setClass("Sensor",
        slots = list(ox = "matrix", 
                red = "matrix", E0 = "numeric"))

# ----------- Sensor constructors
#' A function to construct a Sensor class based on two matricies
#' @param oxMatrix A 2XN numeric matrix
#' @param redMatrix A 2xN numeric matrix
#' @param E0 a numeric value for the midpoint potential of the sensor
#' @return A sensor object
Sensor <- function(oxMatrix, redMatrix, E0) {
    return(new("Sensor", ox = oxMatrix,
        red = redMatrix, E0 = E0));
}

#' An additional function-constructor for seperate x and y arrays
#' @param xOx A numeric array representing wavelength 
#' values in an oxidized state
#' @param yOx A numeric array representing relative absorption 
#' at the wavelengths of xOx
#' @param xRed A numeric array representing wavelength 
#' values in a reduced state
#' @param yRed A numeric array representing relative absorption
#' at the wavelenghts of xRed
#' @param E0 a numeric value for the midpoint potential of the sensor
#' @return A sensor object
SensorPoint <- function(xOx, yOx, xRed, yRed, E0) {
    return(Sensor(createPos(xOx, yOx), createPos(xRed, yRed), E0));
}

#' A function to construct a sensor class based on sensor properties
#' @param Rmax the maximum ratiometric intensity value
#' @param Rmin the minimum ratiometric intensity value
#' @param delta the dynamic range of the divisor wavelength
#' @param lambda_1 (optional) dividend wavelength, default = 410
#' @param lambda_2 (optional) divisor wavelength, default = 470
#' @param E0 (optional) midpoint potential of the sensor, 
#' default=-265
#' @usage SensorValues(Rmax, Rmin, delta,
#' lambda_1 = 410, lambda_2 = 470, E0 = -265)
#' @return A sensor object
SensorValues <- function(Rmax, Rmin, delta, 
                        lambda_1 = 410, lambda_2 = 470, E0 = -265) {
    return(Sensor(createPos(c(lambda_1, lambda_2),c(Rmax*delta, delta)), 
        createPos(c(lambda_1, lambda_2), c(Rmin, 1) ), E0)); 
}

