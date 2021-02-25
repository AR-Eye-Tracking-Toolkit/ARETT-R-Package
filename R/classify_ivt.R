# Copyright (c) Sebastian Kapp.
# Licensed under the MIT License.

#' Classify Fixations by Velocity Threshold
#'
#' @description
#' Classify each gaze point based on their velocities as calculated by \code{\link[arett]{calculate_velocity}}.
#'
#' @details
#' This is an implementation of the fixation identification by velocity threshold (I-VT) as documented in the \href{https://www.tobiipro.com/siteassets/tobii-pro/learn-and-support/analyze/how-do-we-classify-eye-movements/tobii-pro-i-vt-fixation-filter.pdf}{Tobii Pro I-VT fixation filter}.
#' It compares the velocity of each gaze point against the threshold and classifies the gaze point as saccade if the velocity is equal or above it.
#' Additionally it groups gaze points of the same classification into an event.
#'
#' The default velocity threshold is 100 degrees per second, as described by Tobii for their mobile eye trackers.
#' For stationary eye trackers the default threshold by Tobii is 30 degrees per second.
#'
#' @format Input data frame columns
#' \describe{
#'   \item{velocity}{Velocity of the current gaze point}
#' }
#'
#' @format Additional columns in output
#' \describe{
#'   \item{classification}{Classification of this gaze point}
#'   \item{eventIndex}{Index of the event in which the gaze point lies}
#'   \item{eventType}{Type of the current event}
#'   \item{eventDuration}{Duration of the current event}
#'   \item{fixation_x}{X coordinate of the current fixation (if eventType is a fixation)}
#'   \item{fixation_y}{Y coordinate of the current fixation (if eventType is a fixation)}
#'   \item{fixation_z}{Z coordinate of the current fixation (if eventType is a fixation)}
#' }
#'
#' @param data Data frame of the eye tracking data we want to process
#' @param velocity_threshold Velocity threshold in degrees per second (°/s) over which the gaze point should be classified as saccade
#' @return The input data frame with the classification columns added
#'
#' @export
classify_ivt <- function(data, velocity_threshold = 100) {
  # List of rows which correspond to the current classification
  lastEventRow <- 1
  currentRows <- c()
  currentType <- ""
  index <- 1

  # classification columns
  data$classification <- NA # Classification of this gaze point
  data$eventIndex <- NA # Duration of the current event
  data$eventType <- NA # Duration of the current event
  data$eventDuration <- NA # Duration of the current event
  data$fixation_x <- NA # x coordinate of the fixation event identified
  data$fixation_y <- NA # y coordinate of the fixation event identified
  data$fixation_z <- NA # z coordinate of the fixation event identified

  # Go trough all rows and classify them
  for (row in 1:nrow(data)) {

    ###
    # First do the classification itself

    # Check if we have a velocity for this row
    if (!is.na(data$velocity[row])) {
      # Check if the velocity is larger or equal to the threshold
      if (data$velocity[row] >= velocity_threshold) {
        data$classification[row] = "saccade"
      } else {
        data$classification[row] = "fixation"
      }
    } else {
      # If we don't have a velocity it is a gap
      data$classification[row] = "gap"
    }

    ###
    # Then analyze the classification for events

    # If we are in the first line we always start a new event
    if (row == 1) {
      currentRows <- c(row)
      currentType <- data$classification[row]
    } else {
      # Otherwise we check if the classification differs from the previous one or if we reached the end of the data
      if (data$classification[row] != data$classification[row - 1] || row == nrow(data)) {
        # If yes, we just completed a event and should save it

        # If we are at the end of the data the last line should also be included in the calculations
        if (row == nrow(data)) {
          currentRows <- c(currentRows, row)
        }

        # Save the event index
        data$eventIndex[currentRows] <- index

        # And the event type
        data$eventType[currentRows] <- currentType

        # Duration is the time from the last sample before the just completed event and the first sample in the new event
        # Note: As we analyze the event in retrospective the current row already is the first sample of a new event
        data$eventDuration[currentRows] <- data$eyeDataRelativeTimestamp[row] - data$eyeDataRelativeTimestamp[lastEventRow]

        # If we have a fixation we have to calculate the position of it which is the average position of all gaze points in this fixation
        if (currentType == "fixation") {
          data$fixation_x[currentRows] <- mean(data$gazePoint_x[currentRows])
          data$fixation_y[currentRows] <- mean(data$gazePoint_y[currentRows])
          data$fixation_z[currentRows] <- mean(data$gazePoint_z[currentRows])
        }

        # After completing the event, reset all flags
        lastEventRow <- row - 1
        currentRows <- c(row)
        currentType <- data$classification[row]
        index <- index + 1
      } else {
        # If have the same classification as the previous lines we simply add the new line to the current ones
        currentRows <- c(currentRows, row)
      }
    }
  }

  # Return the result
  return(data)
}
