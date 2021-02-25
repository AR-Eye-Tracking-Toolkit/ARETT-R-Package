# Copyright (c) Sebastian Kapp.
# Licensed under the MIT License.

#' Classify Fixations by AOI
#'
#' @description
#' Classify each gaze point based on whether it hits an AOI and a minimum fixation duration
#'
#' @details
#' This is a custom implementation of an fixation identification by area of interest.
#' It first classifies all gaze points which hit an AOI as fixation and all other gaze points as saccades.
#' Then it groups consecutive gaze points on the same AOI as fixation when the total duration of gaze points exceed the minimum fixation duration.
#' If the minimum fixation duration isn't exceeded, the gaze points are reclassified as saccades.
#'
#' @format Input data frame columns
#' \describe{
#'   \item{eyeDataRelativeTimestamp}{Timestamp of the data}
#'   \item{gazePoint_x}{X coordinates of the gaze point}
#'   \item{gazePoint_y}{Y coordinates of the gaze point}
#'   \item{gazePoint_z}{Z coordinates of the gaze point}
#'   \item{gazePointAOI_name}{Name of the AOI which was hit}
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
#' @param min_fixation_duration Minimal fixation duration
#' @return The input data frame with the classification columns added
#'
#' @export
classify_iaoi <- function(data, min_fixation_duration = 100) {
  # Initialize the columns for the classification ----

  data$eventIndex <- NA # Duration of the current event
  data$eventType <- NA # Duration of the current event
  data$eventDuration <- NA # Duration of the current event
  data$fixation_x <- NA # x coordinate of the fixation event identified
  data$fixation_y <- NA # y coordinate of the fixation event identified
  data$fixation_z <- NA # z coordinate of the fixation event identified


  # Label all gaze points as fixation when they hit an AOI and as saccade otherwise ----

  data$eventType[is.na(data$gazePointAOI_name)] <- "saccade"
  data$eventType[!is.na(data$gazePointAOI_name)] <- "fixation"


  # Group consecutive fixation points, add an event index and the event duration and reclassify the event if the fixation duration is too short ----

  # We start the first event with the first row
  eventIndex <- 1
  eventRows <- c(1)
  lastRowPreviousEvent <- 1
  currentEventType <- data$eventType[1]
  currentAOIname <- data$gazePointAOI_name[1]


  # Go through all rows
  nRows <- nrow(data)
  for (row in 2:nRows) {
    # If we have a new event type or a new AOI the previous fixation has ended (or we are at the end of the file)
    if (
      data$eventType[row] != currentEventType
      ||
      (
        (
          # Note: The AOI name can be NA which we can't compare using != so we need to test it separately
          is.na(data$gazePointAOI_name[row]) && !is.na(currentAOIname)
        )
        ||
        (
          !is.na(data$gazePointAOI_name[row]) &&
          (
            is.na(currentAOIname) || data$gazePointAOI_name[row] != currentAOIname
          )
        )
      )
      ||
      row == nRows
    ) {
      # If we are in the last row add this row to the current list and increase the counter +1 to fully calculate the last fixation
      if (row == nRows) {
        eventRows <- append(eventRows, row)
        row <- row + 1
      }

      # Complete the previous event by writing the index
      data$eventIndex[eventRows] <- eventIndex

      # The event duration is the time between the last sample of the previous event and the last sample in this event
      # Note 1: Starting at the end of the last event to prevent gaps
      # Note 2: In the first event we don't have a previous event so we start at the first row.
      #         This is already handled by starting the loop at the second row and setting the lastRowPreviousEvent to one at the beginning
      data$eventDuration[eventRows] <- data$eyeDataRelativeTimestamp[row - 1] - data$eyeDataRelativeTimestamp[lastRowPreviousEvent]

      # If the event is a fixation check if the duration is long enough, otherwise reclassify as saccade
      if (data$eventType[row - 1] == "fixation" && data$eventDuration[row - 1] < min_fixation_duration) {
        data$eventType[eventRows] <- "saccade"
      }

      # if the event is still a fixation calculate the fixation position as average position of the gaze point
      if (data$eventType[row - 1] == "fixation") {
        data$fixation_x[eventRows] <- mean(na.omit(data$gazePoint_x[eventRows]))
        data$fixation_y[eventRows] <- mean(na.omit(data$gazePoint_y[eventRows]))
        data$fixation_z[eventRows] <- mean(na.omit(data$gazePoint_z[eventRows]))
      }

      # Start a new event with the current row
      lastRowPreviousEvent <- row - 1
      eventIndex <- eventIndex + 1
      eventRows <- c(row)
      currentEventType <- data$eventType[row]
      currentAOIname <- data$gazePointAOI_name[row]
    }
    # Otherwise we add the current row to the current event
    else {
      eventRows <- append(eventRows, row)
    }
  }

  return(data)
}
