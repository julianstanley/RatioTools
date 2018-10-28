#' An S4 class to represent a sensor
#'
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
# Main constructor
# ARGS: Two, 2-by-N matricies 
# -- oxMatrix: matrix of x and y values in oxidized state
# -- redMatrix: matrix of x and y values in reduced state


#' A function to construct a Sensor class based on two matricies
#' @param oxMatrix A 2XN numeric matrix
#' @param redMatrix A 2xN numeric matrix
#' @param E0 a numeric value for the midpoint potential of the sensor
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
#' @param E0 a numeric value for the midpoint potential of the sensor
SensorPoint <- function(xOx, yOx, xRed, yRed, E0) {
    return(Sensor(createPos(xOx, yOx), createPos(xRed, yRed), E0));
}

# Additional constructor for Rmax, Rmin, and delta
# Helps by making vectors corresponding to values given
SensorValues <- function(Rmax, Rmin, delta, 
                        lambda_1 = 410, lambda_2 = 470, E0 = -265) {
    return(Sensor(createPos(c(lambda_1, lambda_2),c(Rmax*delta, delta)), 
        createPos(c(lambda_1, lambda_2), c(Rmin, 1) ), E0)); 
}

