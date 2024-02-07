# Copyright (c) Sebastian Kapp.
# Licensed under the MIT License.

#' Calculate Velocity
#'
#' @description
#' Calculate the velocity of each gaze point (results required for I-VT fixation filter)
#'
#' @details
#' This is an implementation of the velocity calculation as documented in the \href{https://www.tobiipro.com/siteassets/tobii-pro/learn-and-support/analyze/how-do-we-classify-eye-movements/tobii-pro-i-vt-fixation-filter.pdf}{Tobii Pro I-VT fixation filter}.
#' It first calculates the window size (number of data points) over which the velocity should be calculated based on the specified window length (in ms)
#' and the average time between data points of the first 100 samples. This accommodates data logs with non-uniform time between data points.
#' Then it calculates the velocity for each gaze point by calculating the angle between the first and last gaze point in the current window
#' with the origin of the gaze point in the middle of the window. This angle divided by the window length is saved as the velocity of the middle gaze point.
#'
#' The default window length of 20ms and the eye tracker rate of the Microsoft HoloLens 2 of 30Hz results in a window size of two and therefore a
#' calculation of the velocity between the previous and current gaze point.
#'
#' @format Input data frame columns
#' \describe{
#'   \item{gazeHasValue}{Logical (boolean) if there is valid gaze data}
#'   \item{eyeDataRelativeTimestamp}{Timestamp of the data}
#'   \item{gazeorigin_x}{X coordinates of the gaze origin}
#'   \item{gazeorigin_y}{Y coordinates of the gaze origin}
#'   \item{gazeorigin_z}{Z coordinates of the gaze origin}
#'   \item{gazePoint_x}{X coordinates of the gaze point}
#'   \item{gazePoint_y}{Y coordinates of the gaze point}
#'   \item{gazePoint_z}{Z coordinates of the gaze point}
#' }
#'
#'
#' @param data Data frame of the eye tracking data we want to process
#' @param window_length Length of the window over which we want to calculate the velocity
#' @return The input data frame with the additional column \emph{velocity}
#'
#' @export
calculate_velocity <- function(data, window_length = 20) {
  # Calculate how many samples are to be analyzed using the window length in ms ----

  # Make sure we have 100 rows which we can analyze, if not analyze all available rows
  maxRow <- 10000
  if (nrow(data) < 10000) maxRow <- nrow(data)

  # Go through the first 100 samples (or less) to find the average time between samples
  sampleTime <- 0
  for (row in 2:maxRow) {
    sampleTime <- sampleTime + (data$eyeDataRelativeTimestamp[row] - data$eyeDataRelativeTimestamp[row-1])
  }
  sampleTime <- sampleTime / maxRow

  # Divide the velocity window by this sample time to get the number of samples we will analyze
  windowCount <- sampleTime / window_length
  # Add 1 and then round down to get final window size
  windowCount <- floor(windowCount + 1)

  # Small cleanup
  rm(maxRow, row, sampleTime)


  # calculate window borders ----

  windowLower <- 0
  windowUpper <- 0

  if (windowCount %% 2 != 0) {
    # If the window count is odd, we can simply subtract one and divide by two to get the offset for the lower and upper limit for the window
    windowUpper <- (windowCount - 1) / 2
    windowLower <- windowUpper
  } else {
    # Otherwise the lower limit is half of the window and the upper limit is one less than the lower limit
    windowLower <- windowCount / 2
    windowUpper <- windowLower - 1
  }


  # Calculate velocities ----

  # How many valid samples did we get so far?
  nValidSamplesWindow <- 0

  # We now go through all gaze points and calculate the velocities.
  for (row in 1:nrow(data)) {
    # If the data is valid and we haven't reached enough valid data to fill the window size yet, increase the number of valid samples we have seen
    # Note: We also have to check for an existing gaze position as we can't guarantee that one exists!
    if (data$gazeHasValue[row] && nValidSamplesWindow < windowCount) {
      nValidSamplesWindow <- nValidSamplesWindow + 1
    } else if (!data$gazeHasValue[row]) {
      # If we don't have a value reset our counter
      nValidSamplesWindow <- 0
    }

    # If we have enough valid samples up to this point calculate the velocity in the middle of the window
    if (nValidSamplesWindow >= windowCount) {
      # Limits of the current window
      upperRow <- row
      sampleRow <- upperRow - windowUpper
      lowerRow <- sampleRow - windowLower

      # Get the head (eye) position from the sample row
      origin_x <- data$gazeOrigin_x[sampleRow]
      origin_y <- data$gazeOrigin_y[sampleRow]
      origin_z <- data$gazeOrigin_z[sampleRow]

      # Get the gaze position at the lower end of the window
      lowerGaze_x <- data$gazePoint_x[lowerRow]
      lowerGaze_y <- data$gazePoint_y[lowerRow]
      lowerGaze_z <- data$gazePoint_z[lowerRow]

      # Get the gaze position at the upper end of the window
      upperGaze_x <- data$gazePoint_x[upperRow]
      upperGaze_y <- data$gazePoint_y[upperRow]
      upperGaze_z <- data$gazePoint_z[upperRow]

      # Calculate the direction of the gaze in form of the vector between the head and gaze position
      lowerDirection_x <- data$gazeOrigin_x[sampleRow] - data$gazePoint_x[lowerRow]
      lowerDirection_y <- data$gazeOrigin_y[sampleRow] - data$gazePoint_y[lowerRow]
      lowerDirection_z <- data$gazeOrigin_z[sampleRow] - data$gazePoint_z[lowerRow]
      upperDirection_x <- data$gazeOrigin_x[sampleRow] - data$gazePoint_x[upperRow]
      upperDirection_y <- data$gazeOrigin_y[sampleRow] - data$gazePoint_y[upperRow]
      upperDirection_z <- data$gazeOrigin_z[sampleRow] - data$gazePoint_z[upperRow]

      # Calculate the scalar product of the two vectors
      scalar <- lowerDirection_x * upperDirection_x + lowerDirection_y * upperDirection_y + lowerDirection_z * upperDirection_z

      # Calculate the length of the two vectors
      lowerDirection_length <- sqrt(lowerDirection_x * lowerDirection_x + lowerDirection_y * lowerDirection_y + lowerDirection_z * lowerDirection_z)
      upperDirection_length <- sqrt(upperDirection_x * upperDirection_x + upperDirection_y * upperDirection_y + upperDirection_z * upperDirection_z)

      # Calculate the angle between the two vectors using the scalar product and their lengths (result in rad)
      angleRad <- acos(scalar / (lowerDirection_length * upperDirection_length))

      # The final angle is supposed to be in degrees instead of rad, therefore we translate the value
      angleDeg <- (angleRad * 180) / pi

      # The time delta is the time between the upper and lower window
      timeDeltaMs <- data$eyeDataRelativeTimestamp[upperRow] - data$eyeDataRelativeTimestamp[lowerRow]

      # We need the time in seconds instead of ms to get the correct velocity unit
      timeDeltaS <- timeDeltaMs / 1000

      # The velocity is now the angle over time
      data$velocity[sampleRow] <- angleDeg / timeDeltaS
    }
  }

  # Return the result
  return(data)
}
