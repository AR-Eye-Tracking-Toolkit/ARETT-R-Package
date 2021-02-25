# Copyright (c) Sebastian Kapp.
# Licensed under the MIT License.

#' Merge Fixations from I-DT Classifier
#'
#' @description
#' Merge fixations classified by \code{\link[arett]{classify_idt}} into a new, combined fixation when they are closer to each other than the max time and the max dispersion specified is not exceeded.
#'
#' @details
#' This is a custom implementation for merging fixations on the same AOI with a gap smaller than the specified time between them.
#' It is intended to be used together with the I-DT algorithm described by \href{https://doi.org/10.3390/s20174956}{Llanes-Jurado et al. (2020, doi:10.3390/s20174956)} and as implemented in \code{\link[arett]{classify_idt}}.
#'
#' @format Input data frame columns
#' \describe{
#'   \item{gazeorigin_x}{X coordinates of the gaze origin}
#'   \item{gazeorigin_y}{Y coordinates of the gaze origin}
#'   \item{gazeorigin_z}{Z coordinates of the gaze origin}
#'   \item{gazePoint_x}{X coordinates of the gaze point}
#'   \item{gazePoint_y}{Y coordinates of the gaze point}
#'   \item{gazePoint_z}{Z coordinates of the gaze point}
#'   \item{classification}{Classification of this gaze point}
#'   \item{eventIndex}{Index of the event in which the gaze point lies}
#'   \item{eventType}{Type of the current event}
#'   \item{eventDuration}{Duration of the current event}
#'   \item{fixation_x}{X coordinate of the current fixation}
#'   \item{fixation_y}{Y coordinate of the current fixation}
#'   \item{fixation_z}{Z coordinate of the current fixation}
#' }
#'
#' @param data Data frame of the eye tracking data we want to process
#' @param max_time Max time between fixations in ms
#' @param dispersion_threshold Dispersion threshold for the combined fixation in degrees
#' @return The input data frame with the fixations merged
#'
#' @export
merge_fixations_idt <- function(data, max_time = 75, dispersion_threshold = 1.6) {
  # Get all event ids and remove all missing ids
  allEventIds <- unique(data$eventIndex)
  allEventIds <- allEventIds[!is.na(allEventIds)]

  # Duplicate the list of all event ids so we can update the list without influencing the loop
  currentEventIds <- allEventIds

  # Remember the original event types
  originalEventTypes <- data$eventType

  # We go through all events using the event ids and if the event isn't a fixation check for the duration of the event
  for (eventId in allEventIds) {
    # Make sure we aren't in the first or last event as they can't be a gap between two fixations
    if (eventId == 1 || eventId == allEventIds[length(allEventIds)]) {
      next
    }

    # As the info about the event is in every line with this id we can simply use the first line of the event
    eventInfo <- data[match(eventId, data$eventIndex), c("eventType", "eventDuration")]

    # Check if the event isn't a fixation and the duration is smaller than the limit
    if (!is.na(eventInfo$eventType) && (eventInfo$eventType != "fixation") && (eventInfo$eventDuration < max_time)) {
      # If this is the case we now need to check if we are actually between to fixations

      # The previous and next event ids
      # Note: We can not simply use -1 and +1 of the id as we might have merged before this event which creates gaps in the event ids
      currentEventIdRow <- match(eventId, currentEventIds)
      previosEventId <- currentEventIds[currentEventIdRow - 1]
      nextEventId <- currentEventIds[currentEventIdRow + 1]

      # Get the last line from the previous event
      eventInfoPrevious <- data[max(which(data$eventIndex == nextEventId)), c("eventType", "gazePointAOI_name")]

      # And the first line of the next event
      eventInfoNext <- data[match(nextEventId, data$eventIndex), c("eventType", "gazePointAOI_name")]

      # Check if we are between two fixations
      # Note: As we only merge fixations we ignore all other cases
      if (eventInfoPrevious$eventType == "fixation" && eventInfoNext$eventType == "fixation") {

        # Get all gaze points in the two fixations we might to merge
        combinedGazePoints <- data[match(nextEventId, data$eventIndex) | match(previosEventIdEventId, data$eventIndex), c("eventType", "gazePointAOI_name")]

        # At first we assume our window covers a fixation
        idt_maxDispersionExceeded <- FALSE

        # Get the mean gaze origin over both fixations for the angle calculations
        idt_meanGazeOrigin_x <- mean(combinedGazePoints$gazeOrigin_x)
        idt_meanGazeOrigin_y <- mean(combinedGazePoints$gazeOrigin_y)
        idt_meanGazeOrigin_z <- mean(combinedGazePoints$gazeOrigin_z)

        # Check the angle between every point in the current window against the limit
        for (i in 1:(nrow(combinedGazePoints)-1)) {
          # If we already exceeded our max dispersion angle we can stop our search
          if (idt_maxDispersionExceeded) {
            break
          }

          # Direction vector for the first point
          idt_iVector_x <- idt_meanGazeOrigin_x - combinedGazePoints$gazePoint_x[i]
          idt_iVector_y <- idt_meanGazeOrigin_y - combinedGazePoints$gazePoint_y[i]
          idt_iVector_z <- idt_meanGazeOrigin_z - combinedGazePoints$gazePoint_z[i]

          for (j in (i+1):nrow(combinedGazePoints)) {
            # Direction vector for the second point
            idt_jVector_x <- idt_meanGazeOrigin_x - combinedGazePoints$gazePoint_x[j]
            idt_jVector_y <- idt_meanGazeOrigin_y - combinedGazePoints$gazePoint_y[j]
            idt_jVector_z <- idt_meanGazeOrigin_z - combinedGazePoints$gazePoint_z[j]

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

              # Break the comparison of the two fixations as we exceeded the angle
              # As we set the corresponding flag this also breaks the search for all other points
              break
            }
          }
        }

        if (!idt_maxDispersionExceeded) {
          # Only if we did not exceeded the max dispersion angle we merge the two fixations
          # For this we extend the previous event and merge the gap as well as the following fixation into one

          # The rows that form the new fixation event
          newEventRows <- data$eventIndex >= previosEventId & data$eventIndex <= nextEventId

          # The new fixation position is the average position of the two fixations we are merging (but not the positions during a saccade)
          fixationRows <- originalEventTypes[newEventRows] == "fixation"
          data$fixation_x[newEventRows] <- mean(data$gazePoint_x[newEventRows][fixationRows])
          data$fixation_y[newEventRows] <- mean(data$gazePoint_y[newEventRows][fixationRows])
          data$fixation_z[newEventRows] <- mean(data$gazePoint_z[newEventRows][fixationRows])

          # The duration is simply the sum of the three events we are merging
          data$eventDuration[newEventRows] <- sum(unique(data$eventDuration[newEventRows]))

          # The event type is a fixation
          data$eventType[newEventRows] <- "fixation"

          # The new event index is the one of the first event
          data$eventIndex[newEventRows] <- previosEventId

          # With this we have overwritten the current and next event id and therefore removed them from existence
          # Update the list of ids accordingly
          currentEventIds <- currentEventIds[currentEventIds != eventId & currentEventIds != nextEventId]
        }
      }
    }
  }

  # Return the result
  return(data)
}
