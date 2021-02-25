# Copyright (c) Sebastian Kapp.
# Licensed under the MIT License.

#' Classify Fixations by Dispersion Threshold
#'
#' @description
#' Classify each gaze point based on the dispersion of gaze points.
#'
#' @details
#' This is an implementation of the fixation classification by dispersion threshold as described by \href{https://doi.org/10.3390/s20174956}{Llanes-Jurado et al. (2020, doi:10.3390/s20174956)}.
#' It uses the distance-dispersion threshold to identify fixations with the specified threshold and minimum duration.
#'
#' According to Llanes-Jurado et al. the acceptable parameter ranges for a HTC Vive Pro Eye HMD is between 1-1.6 degrees and 0.25-0.4 seconds
#' with 1 degree and 0.25 seconds being the optimum.
#' Due to the different precision of the HoloLens 2 eye tracker these thresholds most likely can't be transferred without adaption.
#'
#' @format Input data frame columns
#' \describe{
#'   \item{gazeHasValue}{Boolean if there is valid gaze data}
#'   \item{eyeDataRelativeTimestamp}{Timestamp of the data}
#'   \item{gazeorigin_x}{X coordinates of the gaze origin}
#'   \item{gazeorigin_y}{Y coordinates of the gaze origin}
#'   \item{gazeorigin_z}{Z coordinates of the gaze origin}
#'   \item{gazePoint_x}{X coordinates of the gaze point}
#'   \item{gazePoint_y}{Y coordinates of the gaze point}
#'   \item{gazePoint_z}{Z coordinates of the gaze point}
#' }
#'
#' @format Additional columns in output
#' \describe{
#'   \item{eventIndex}{Index of the event in which the gaze point lies}
#'   \item{eventType}{Type of the current event}
#'   \item{eventDuration}{Duration of the current event}
#'   \item{fixation_x}{X coordinate of the current fixation (if eventType is a fixation)}
#'   \item{fixation_y}{Y coordinate of the current fixation (if eventType is a fixation)}
#'   \item{fixation_z}{Z coordinate of the current fixation (if eventType is a fixation)}
#' }
#'
#'
#' @param data Data frame of the eye tracking data we want to process
#' @param dispersion_threshold Dispersion threshold in degrees
#' @param time_window Time window in ms (minimum fixation duration)
#' @return The input data frame with the classification columns added
#'
#' @export
classify_idt <- function(data, dispersion_threshold = 1.6, time_window = 250) {
  # Calculate how many samples are to be analyzed using the window length in ms

  # Make sure we have 100 rows which we can analyze ----
  idt_maxRow <- 100
  if (nrow(data) < 100) idt_maxRow <- nrow(data)

  # Go through the first 100 samples (or less) to find the average time between samples
  idt_sampleTime <- 0
  for (idt_row in 2:idt_maxRow) {
    idt_sampleTime <- idt_sampleTime + (data$eyeDataRelativeTimestamp[idt_row] - data$eyeDataRelativeTimestamp[idt_row-1])
  }
  idt_sampleTime <- idt_sampleTime / idt_maxRow

  # Divide the window by this sample time to get the number of samples we will analyze
  idt_minWindowCount <- time_window / idt_sampleTime
  # Add 1 and then round down to get final window size
  idt_minWindowCount <- floor(idt_minWindowCount + 1)

  # Small cleanup
  rm(idt_maxRow, idt_row, idt_sampleTime)


  # Initialize the calculation with an initial window ----

  # We start at the beginning of the file
  idt_lowerWindow <- 1
  # And add the min window size to get our initial window
  idt_upperWindow <- idt_lowerWindow + (idt_minWindowCount - 1)


  # Initialize the classification variables ----

  # Index of the current fixation
  idt_fixationIndex <- 1
  idt_lastFixationRow <- NA

  # classification columns
  data$eventIndex <- NA # Duration of the current event
  data$eventType <- NA # Duration of the current event
  data$eventDuration <- NA # Duration of the current event
  data$fixation_x <- NA # x coordinate of the fixation event identified
  data$fixation_y <- NA # y coordinate of the fixation event identified
  data$fixation_z <- NA # z coordinate of the fixation event identified


  # Main calculation loop of the classification ----

  # Loop until we have exceeded our data
  while (idt_upperWindow <= nrow(data)) {

    # Check if we have a gaze point for all rows in the current window
    idt_allGazePointsValid <- TRUE

    # For this we start at the end of the window and go forward to catch the last invalid point
    for (row in c(idt_upperWindow:idt_lowerWindow)) {
      if (is.na(data$gazePoint_x[row])) {
        # If we have an invalid row, we can't identify a fixation and we have to move our window
        idt_allGazePointsValid <- FALSE

        # For this we start directly after the last invalid gaze point with the min window size and check again
        idt_lowerWindow <- row + 1
        idt_upperWindow <- idt_lowerWindow + (idt_minWindowCount - 1)

        # We can stop the check here
        break
      }
    }
    rm(row)

    # If we have invalid gaze points restart the check
    # Note: We have already set the new window!
    if (!idt_allGazePointsValid) {
      next
    }

    # We now know that we have valid gaze data for all points in the current window

    # At first we assume our window covers a fixation
    idt_maxDispersionExceeded <- FALSE

    # Get the mean gaze origin for the angle calculations
    idt_meanGazeOrigin_x <- mean(data$gazeOrigin_x[c(idt_lowerWindow:idt_upperWindow)])
    idt_meanGazeOrigin_y <- mean(data$gazeOrigin_y[c(idt_lowerWindow:idt_upperWindow)])
    idt_meanGazeOrigin_z <- mean(data$gazeOrigin_z[c(idt_lowerWindow:idt_upperWindow)])

    ###
    # Check the angle between every point in the current window against the limit
    for (i in idt_lowerWindow:(idt_upperWindow-1)) {
      # If we already exceeded our max dispersion angle we can stop our search
      if (idt_maxDispersionExceeded) {
        break
      }


      # Direction vector for the first point
      idt_iVector_x <- idt_meanGazeOrigin_x - data$gazePoint_x[i]
      idt_iVector_y <- idt_meanGazeOrigin_y - data$gazePoint_y[i]
      idt_iVector_z <- idt_meanGazeOrigin_z - data$gazePoint_z[i]

      for (j in (i+1):idt_upperWindow) {
        # Direction vector for the second point
        idt_jVector_x <- idt_meanGazeOrigin_x - data$gazePoint_x[j]
        idt_jVector_y <- idt_meanGazeOrigin_y - data$gazePoint_y[j]
        idt_jVector_z <- idt_meanGazeOrigin_z - data$gazePoint_z[j]

        # Calculate the scalar product of the two vectors
        idt_scalar <- idt_iVector_x * idt_jVector_x + idt_iVector_y * idt_jVector_y + idt_iVector_z * idt_jVector_z

        # Calculate the length of the two vectors
        idt_iVector_length <- sqrt(idt_iVector_x * idt_iVector_x + idt_iVector_y * idt_iVector_y + idt_iVector_z * idt_iVector_z)
        idt_jVector_length <- sqrt(idt_jVector_x * idt_jVector_x + idt_jVector_y * idt_jVector_y + idt_jVector_z * idt_jVector_z)

        # Calculate the angle between the two vectors using the scalar product and their lengths (result in rad)
        idt_angleRad <- acos(idt_scalar / (idt_iVector_length * idt_jVector_length))

        # The final angle is supposed to be in degrees instead of rad, therefore we translate the value
        idt_angleDeg <- (idt_angleRad * 180) / pi

        # Check the angle against the limit
        if (idt_angleDeg > dispersion_threshold) {
          # If we exceeded the limit we can stop checking for this window
          idt_maxDispersionExceeded <- TRUE

          # We now know that the first point is too far apart from the second vector.
          # As we are in the initial window we wouldn't have the required minimum fixation duration if we would discard everything after the second point
          # Therefore we start a new initial window one point after the "current first point" and check again
          idt_lowerWindow <- i + 1
          idt_upperWindow <- idt_lowerWindow + (idt_minWindowCount - 1)

          # Break the comparison of the current first point with second points as we exceeded the angle
          # As we set the corresponding flag this also breaks the search for all other points
          break
        }
      }
    }

    # If we exceeded the max dispersion angle in our search we have to restart the search
    # Note: The new lower and upper window limits have already been set so we don't need to do anything here!
    if (idt_maxDispersionExceeded) {
      next
    }

    ###
    # We now have a window that is exactly the minimum size and does not exceed the max dispersion angle
    # Now we can add additional points to our window until the new point exceeds our dispersion angle

    # Reset the exceeded flag (should already be false but to make sure)
    idt_maxDispersionExceeded <- FALSE

    while (!idt_maxDispersionExceeded) {
      # Add a new point to our window
      idt_upperWindow <- idt_upperWindow + 1

      # Make sure we are still inside our data
      if (idt_upperWindow > nrow(data)) {
        # Treat exceeding the number of data rows the same as exceeding the max dispersion
        # This triggers the final calculation and assignment of the fixation
        # After this calculation the window is moved which again results in exceeding of the number of rows
        #    which is caught in the main loop and causes it to end.
        # Note: This leaves the window with one point too much which we will correct later
        idt_maxDispersionExceeded <- TRUE
        break
      }

      # Make sure the new line actually contains data
      if (is.na(data$gazePoint_x[idt_upperWindow])) {
        # Treat reaching invalid data the same as exceeding the max dispersion
        # This triggers the final calculation and assignment of the fixation
        # After this calculation the window is moved which then triggers the invalid data check again moving the window until it reaches valid data again
        # Note: This leaves the window with one point too much which we will correct later
        idt_maxDispersionExceeded <- TRUE
        break
      }

      # Check the angle of the new point to all previous points
      # Note: Now the new point is point i while the old, "lower" points are the points j

      # Direction vector for the first point
      idt_iVector_x <- idt_meanGazeOrigin_x - data$gazePoint_x[idt_upperWindow]
      idt_iVector_y <- idt_meanGazeOrigin_y - data$gazePoint_y[idt_upperWindow]
      idt_iVector_z <- idt_meanGazeOrigin_z - data$gazePoint_z[idt_upperWindow]

      for (j in idt_lowerWindow:(idt_upperWindow-1)) {
        # Direction vector for the second point
        idt_jVector_x <- idt_meanGazeOrigin_x - data$gazePoint_x[j]
        idt_jVector_y <- idt_meanGazeOrigin_y - data$gazePoint_y[j]
        idt_jVector_z <- idt_meanGazeOrigin_z - data$gazePoint_z[j]

        # Calculate the scalar product of the two vectors
        idt_scalar <- idt_iVector_x * idt_jVector_x + idt_iVector_y * idt_jVector_y + idt_iVector_z * idt_jVector_z

        # Calculate the length of the two vectors
        idt_iVector_length <- sqrt(idt_iVector_x * idt_iVector_x + idt_iVector_y * idt_iVector_y + idt_iVector_z * idt_iVector_z)
        idt_jVector_length <- sqrt(idt_jVector_x * idt_jVector_x + idt_jVector_y * idt_jVector_y + idt_jVector_z * idt_jVector_z)

        # Calculate the angle between the two vectors using the scalar product and their lengths (result in rad)
        idt_angleRad <- acos(idt_scalar / (idt_iVector_length * idt_jVector_length))

        # The final angle is supposed to be in degrees instead of rad, therefore we translate the value
        idt_angleDeg <- (idt_angleRad * 180) / pi

        # Check the angle against the limit
        if (idt_angleDeg > dispersion_threshold) {
          # We now know that adding the new point will exceed the max dispersion angle, therefore we stop checking.
          # By setting the exceeded flag we also stop adding new points to the window.
          # Note: This leaves the window with one point too much which we will correct later
          idt_maxDispersionExceeded <- TRUE
          break
        }
      }

      # If we did not exceed the max dispersion angle with this point we can simply continue our loop and add more points to the window
    }

    # When we reach this point our loop of adding points to the current window stopped. This can be of three reasons:
    #  a) We reached the end of our data after adding a new point
    #  b) We reached missing data
    #  c) We exceeded the max dispersion angle after adding a new point

    # As we always break the loop after adding a new point, our last valid window is one point smaller than the current state
    idt_upperWindow <- idt_upperWindow - 1

    ###
    # If we previously identified a fixation and have a gap, write an event index into this gap
    if (!is.na(idt_lastFixationRow) && idt_lowerWindow > 1 && idt_lastFixationRow+1 != idt_lowerWindow) {
      # Update the event index for all rows from the last fixation to this one
      data$eventIndex[c((idt_lastFixationRow+1):(idt_lowerWindow-1))] <- idt_fixationIndex
      data$eventType[c((idt_lastFixationRow+1):(idt_lowerWindow-1))] <- NA
      # Increase the index as we just wrote the index for the gap between fixations
      idt_fixationIndex <- idt_fixationIndex + 1
    }

    ###
    # Now we can calculate the identified fixation

    # The rows we are currently looking at are all rows inside our window
    idt_currentFixationRows <- c(idt_lowerWindow:idt_upperWindow)

    # Write the fixation classification into the data
    data$eventIndex[idt_currentFixationRows] <- idt_fixationIndex
    data$eventType[idt_currentFixationRows] <- "fixation"

    # The duration is the time from the last sample before the window and the last sample in this window
    # (Note: Starting before this window to prevent gaps)
    if (idt_lowerWindow == 1) {
      # If this is our first sample we can of course not use the sample before it
      data$eventDuration[idt_currentFixationRows] <- data$eyeDataRelativeTimestamp[idt_upperWindow] - data$eyeDataRelativeTimestamp[idt_lowerWindow]
    }
    else {
      # Otherwise calculate as described
      data$eventDuration[idt_currentFixationRows] <- data$eyeDataRelativeTimestamp[idt_upperWindow] - data$eyeDataRelativeTimestamp[idt_lowerWindow-1]
    }


    # The fixation position is the average position of all gaze points in this fixation
    data$fixation_x[idt_currentFixationRows] <- mean(data$gazePoint_x[idt_currentFixationRows])
    data$fixation_y[idt_currentFixationRows] <- mean(data$gazePoint_y[idt_currentFixationRows])
    data$fixation_z[idt_currentFixationRows] <- mean(data$gazePoint_z[idt_currentFixationRows])

    # Increase the fixation index as we just classified a fixation and note the last row of this fixation
    idt_fixationIndex <- idt_fixationIndex + 1
    idt_lastFixationRow <- idt_upperWindow

    ###
    # Start a new window with the minimum window size directly after the current window
    idt_lowerWindow <- idt_upperWindow + 1
    idt_upperWindow <- idt_lowerWindow + (idt_minWindowCount - 1)

    ###
    # Restart the loop evaluating first the minimal window size and then adding new points again
    # Note: If we exceeded our data with the new window the loop will stop automatically
  }

  return(data)
}
