# Copyright (c) Sebastian Kapp.
# Licensed under the MIT License.

#' Reduce Noise
#'
#' @description
#' This function reduces noise in the eye tracking data by applying the specified method (usually either median or mean) over the specified window
#'
#' @details
#' This is an implementation of the noise reduction function as documented in the \href{https://www.tobiipro.com/siteassets/tobii-pro/learn-and-support/analyze/how-do-we-classify-eye-movements/tobii-pro-i-vt-fixation-filter.pdf}{Tobii Pro I-VT fixation filter}.
#' It goes through all gaze points and replaces each data point by the result of the specified method (in case of Tobii either median or mean) over all data points
#' in a window centered around the data point currently being replaced.
#'
#' As the eye tracking rate of the HoloLens 2 is 30 Hz, a noise reduction over 3 samples would result in a window size of 1/30Hz = 100ms.
#' With the typical fixation duration around 200ms this would result in a window not much smaller than the fixation duration and risk
#' significantly influencing the data negatively. Therefore it should not be used by default.
#'
#' @format Input data frame columns
#' \describe{
#'   \item{gazeorigin_x}{X coordinates of the gaze origin}
#'   \item{gazeorigin_y}{Y coordinates of the gaze origin}
#'   \item{gazeorigin_z}{Z coordinates of the gaze origin}
#'   \item{gazeDirection_x}{X coordinates of the gaze direction}
#'   \item{gazeDirection_y}{Y coordinates of the gaze direction}
#'   \item{gazeDirection_z}{Z coordinates of the gaze direction}
#'   \item{gazePoint_x}{X coordinates of the gaze point}
#'   \item{gazePoint_y}{Y coordinates of the gaze point}
#'   \item{gazePoint_z}{Z coordinates of the gaze point}
#' }
#'
#'
#' @param data Data frame of the eye tracking data we want to process
#' @param method function which is given the current data window and returns the denoised value (usually either median or mean)
#' @param window_size Number of data points over which we want to apply the noise reduction
#' @return The modified input data frame with the noise reduction applied and an additional column \emph{modified} which indicates if the line was modified
#'
#' @export
noise_reduction <- function(data, method = median, window_size = 3) {

  # Add modified column if it doesn't already exist
  if (!("modified" %in% colnames(data))) {
    data$modified <- FALSE
  }

  # Calculate window borders

  # Make sure we have selected a valid window size which is odd
  stopifnot(window_size %% 2 != 0)

  # Calculate upper and lower boundaries
  # Note: As the window must be symmetrical the values are the same
  windowUpper <- (window_size - 1) / 2
  windowLower <- windowUpper

  # Do noise reduction

  # We now go through all gaze points and do the noise reduction
  for (row in 1:nrow(data)) {
    # Identify the window over which we want to apply the noise reduction. The size could be affected by the beginning/end of the data or gaps
    if (is.na(data$gazePoint_x[row])) {
      # If we don't have any data at this row we don't have to do anything
      next
    } else if (row == 1 || row == nrow(data)) {
      # At the very beginning of the data the window size can only be one row and therefore nothing changes
      next
    } else if (row <= windowLower) {
      # If the row number is smaller or equal than the lower window we have to resize our window accordingly
      # The current row is the middle of the window, that means all data up to this point including the row itself are in this window
      window <- c(1:row)
      # Additionally there are as many following rows in the window as there are before the current row
      window <- c(window, c((row + 1) : (row + row - 1)))
    } else if (nrow(data) - row <= windowUpper) {
      # If the row is nearer to the end of the data than the upper window size we also have to resize our window
      # The lower limit of the new window depends on the number of rows left till the end of the data
      window <- c((row - (nrow(data) - row)) : nrow(data))
    } else {
      # If there are no limits due to the number of rows available the window is simply the specified size
      window <- c((row - windowLower) : (row + windowUpper))
    }

    # Now we have to make sure that we actually have gaze data for all rows in the current window and if not, resize the window
    # Note: We only check the x coordinate of the gaze point but if this coordinate exists all other coordinates also exist
    rowsValid <- is.na(data$gazePoint_x[window])
    if (sum(rowsValid) > 0) {
      # We have rows which don't have data in them! Resize the window

      # New window limits
      windowUpperValid <- windowUpper
      windowLowerValid <- windowLower

      # Go through the lower window and check if at some point the data is not valid
      # For this we start at the middle and go to the previous rows until we hit an invalid one or have checked all rows
      for (checkLowerRow in 1:windowLower) {
        if (is.na(data$gazePoint_x[row - checkLowerRow])) {
          # We found an invalid data point and therefore our limit for the lower row is one less than the current size
          windowLowerValid <- checkLowerRow - 1
        }
      }
      # Do the same for the upper window by going from the middle up
      for (checkUpperRow in 1:windowUpper) {
        if (is.na(data$gazePoint_x[row + checkUpperRow])) {
          # We found an invalid data point and therefore our limit for the upper row is one less than the current size
          windowUpperValid <- checkUpperRow - 1
        }
      }

      # If the new lower limit is smaller or equal than the upper limit resize the window to its size
      if (windowLowerValid <= windowUpperValid) {
        window <- c((row - windowLowerValid) : (row + windowLowerValid))
      } else {
        # Otherwise we use the upper limit
        window <- c((row - windowUpperValid) : (row + windowUpperValid))
      }
    }

    # Now we have a window which only contains valid data and we can redefine the coordinates based on the method selected
    data$gazeOrigin_x[row] <- method(data$gazeOrigin_x[window])
    data$gazeOrigin_y[row] <- method(data$gazeOrigin_y[window])
    data$gazeOrigin_z[row] <- method(data$gazeOrigin_z[window])
    data$gazeDirection_x[row] <- method(data$gazeDirection_x[window])
    data$gazeDirection_y[row] <- method(data$gazeDirection_y[window])
    data$gazeDirection_z[row] <- method(data$gazeDirection_z[window])
    data$gazePoint_x[row] <- method(data$gazePoint_x[window])
    data$gazePoint_y[row] <- method(data$gazePoint_y[window])
    data$gazePoint_z[row] <- method(data$gazePoint_z[window])

    # mark as modified
    data$modified[row] <- TRUE
  }

  # Return the result
  return(data)
}
