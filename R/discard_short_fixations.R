# Copyright (c) Sebastian Kapp.
# Licensed under the MIT License.

#' Discard Short Fixations
#'
#' @description
#' Discard fixations which have a duration shorter than the specified minimum duration.
#'
#' @details
#' This function removes all fixations which are shorter than the specified minimum duration and reclassifies them as gaps.
#' If \code{reclassify_saccade} is specified they are classified as saccade instead of a gap.
#' This post processing is only necessary after the I-VT filter as both I-DT as well as I-AOI inherently include a minimum fixation duration.
#'
#' @format Input data frame columns
#' \describe{
#'   \item{eventIndex}{Index of the event in which the gaze point lies}
#'   \item{eventType}{Type of the current event}
#'   \item{eventDuration}{Duration of the current event}
#'   \item{fixation_x}{X coordinate of the current fixation}
#'   \item{fixation_y}{Y coordinate of the current fixation}
#'   \item{fixation_z}{Z coordinate of the current fixation}
#' }
#'
#' @param data Data frame of the eye tracking data we want to process
#' @param min_duration Minimum duration of a fixation
#' @param reclassify_saccade Reclassify discarded fixations as saccade instead of a gap
#' @return The input data frame with the fixation classifications which are too short discarded
#'
#' @export
discard_short_fixations <- function(data, min_duration = 60, reclassify_saccade = FALSE) {
  # Get all event ids and remove all missing ids
  allEventIds <- unique(data$eventIndex)
  allEventIds <- allEventIds[!is.na(allEventIds)]

  # We go through all events using the event ids and if the event is a fixation check for the duration of the event
  for (eventId in allEventIds) {
    # As the info about the event is in every line with this id we can simply use the first line of the id
    eventInfo <- data[data$eventIndex == eventId, ]
    eventInfo <- eventInfo[1, ]

    # Check if we are in a fixation and if the duration of the fixation is smaller than the limit
    if (eventInfo$eventType == "fixation" && eventInfo$eventDuration < min_duration) {
      # If this is the case we want to discard the fixation

      # Change the classification based on whether we want it to become a saccade or a gap
      if (reclassify_saccade) {
        data$eventType[data$eventIndex == eventId] <- "saccade"
      } else {
        data$eventType[data$eventIndex == eventId] <- "gap"
      }

      # Remove the fixation information of the event
      data$fixation_x[data$eventIndex == eventId] <- NA
      data$fixation_y[data$eventIndex == eventId] <- NA
      data$fixation_z[data$eventIndex == eventId] <- NA
    }
  }

  # Return the result
  return(data)
}
