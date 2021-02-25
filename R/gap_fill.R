# Copyright (c) Sebastian Kapp.
# Licensed under the MIT License.

#' Fill Gaps
#'
#' @description
#' This function fills gaps in the eye tracking data by inserting a linear interpolation
#' between the last valid sample before the gap and the first valid sample after the gap.
#'
#' @format Input data frame columns
#' \describe{
#'   \item{gazeHasValue}{Boolean if there is valid gaze data}
#'   \item{eyeDataRelativeTimestamp}{Timestamp of the data}
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
#' @param max_gap_length Maximum gap length in ms which we want to fill
#' @return The modified input data frame with the gaps filled and an additional column \emph{modified} which indicates if the line was modified
#'
#' @export
gap_fill <- function(data, max_gap_length = 75) {
  # Add modified column if it doesn't already exist
  if (!("modified" %in% colnames(data))) {
    data$modified <- FALSE
  }

  lastValidRow <- 0
  wasGap <- FALSE

  for (row in 1:nrow(data)) {
    # If the current data is valid and we weren't in a gap, save the current row as the last valid row
    if (data$gazeHasValue[row] && !wasGap) {
      lastValidRow <- row
    }

    # If the current data is valid but we were in a gap, check if we should fill this gap
    else if (data$gazeHasValue[row] && wasGap) {
      # Check and compare the gap duration
      gapDuration <- data$eyeDataRelativeTimestamp[row] - data$eyeDataRelativeTimestamp[lastValidRow]
      if (gapDuration < max_gap_length) {

        # We have a gap we want to fill, iterate over all rows to be filled
        for (fillRow in (lastValidRow+1):(row-1)) {

          # Calculate the scaling factor
          scalingFactor <- 1 - (data$eyeDataRelativeTimestamp[fillRow] - data$eyeDataRelativeTimestamp[row])/(data$eyeDataRelativeTimestamp[lastValidRow] - data$eyeDataRelativeTimestamp[row])

          # Calculate the missing values
          data$gazeOrigin_x[fillRow] <- (scalingFactor * (data$gazeOrigin_x[row] - data$gazeOrigin_x[lastValidRow])) + data$gazeOrigin_x[lastValidRow]
          data$gazeOrigin_y[fillRow] <- (scalingFactor * (data$gazeOrigin_y[row] - data$gazeOrigin_y[lastValidRow])) + data$gazeOrigin_y[lastValidRow]
          data$gazeOrigin_z[fillRow] <- (scalingFactor * (data$gazeOrigin_z[row] - data$gazeOrigin_z[lastValidRow])) + data$gazeOrigin_z[lastValidRow]
          data$gazeDirection_x[fillRow] <- (scalingFactor * (data$gazeDirection_x[row] - data$gazeDirection_x[lastValidRow])) + data$gazeDirection_x[lastValidRow]
          data$gazeDirection_y[fillRow] <- (scalingFactor * (data$gazeDirection_y[row] - data$gazeDirection_y[lastValidRow])) + data$gazeDirection_y[lastValidRow]
          data$gazeDirection_z[fillRow] <- (scalingFactor * (data$gazeDirection_z[row] - data$gazeDirection_z[lastValidRow])) + data$gazeDirection_z[lastValidRow]
          data$gazePoint_x[fillRow] <- (scalingFactor * (data$gazePoint_x[row] - data$gazePoint_x[lastValidRow])) + data$gazePoint_x[lastValidRow]
          data$gazePoint_y[fillRow] <- (scalingFactor * (data$gazePoint_y[row] - data$gazePoint_y[lastValidRow])) + data$gazePoint_y[lastValidRow]
          data$gazePoint_z[fillRow] <- (scalingFactor * (data$gazePoint_z[row] - data$gazePoint_z[lastValidRow])) + data$gazePoint_z[lastValidRow]

          # mark as modified
          data$modified[fillRow] <- TRUE
        }
      }

      # We just reached the end of a gap and this row was valid
      wasGap <- FALSE
      lastValidRow <- row
    }

    # If the data isn't valid we are in a gap
    else {
      wasGap <- TRUE
    }
  }

  # Return the result
  return(data)
}
