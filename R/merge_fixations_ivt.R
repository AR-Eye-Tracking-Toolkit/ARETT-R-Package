# Copyright (c) Sebastian Kapp.
# Licensed under the MIT License.

#' Merge Fixations from I-VT Classifier
#'
#' @description
#' Merge fixations classified by \code{\link[arett]{classify_ivt}} into a new, combined fixation when they are closer to each other than the max time and max angle distance specified.
#'
#' @details
#' This is an implementation of the merge fixation step as documented in the \href{https://www.tobiipro.com/siteassets/tobii-pro/learn-and-support/analyze/how-do-we-classify-eye-movements/tobii-pro-i-vt-fixation-filter.pdf}{Tobii Pro I-VT fixation filter}.
#' It checks each gap between fixation if it is shorter than the specified max time between fixations
#' and the angle between the fixations before and after the gap is smaller than specified.
#' If this is the case the two fixations and the gap are merged together into a new, combined fixation.
#'
#' The default values for max time and max angle are defined by Tobii to be 75 ms and 0.5 degrees.
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
#' @param max_angle Max angle between fixations in degrees
#' @return The input data frame with the fixations merged
#'
#' @export
merge_fixations_ivt <- function(data, max_time = 75, max_angle = 0.5) {
  # Get all event ids and remove all missing ids
  allEventIds <- unique(data$eventIndex)
  allEventIds <- allEventIds[!is.na(allEventIds)]

  # Remember the original event types
  originalEventTypes <- data$eventType

  # We go through all events using the event ids and if the event isn't a fixation check for the duration of the event
  for (eventId in allEventIds) {
    # Make sure we aren't in the first or last event as they can't be a gap between two fixations
    if (eventId == 1 || eventId == allEventIds[length(allEventIds)]) {
      next
    }

    # As the info about the event is in every line with this id we can simply use the first line of the event
    eventInfo <- data[data$eventIndex == eventId, ]
    eventInfo <- eventInfo[1, ]

    # Check if the event isn't a fixation and the duration is smaller than the limit
    if ((eventInfo$classification != "fixation") && (eventInfo$eventDuration < max_time)) {
      # If this is the case we now need to check if we are actually between to fixations

      # The previous and next event ids
      # Note: we can not simply use -1 and +1 of the id as we might have merged before this event which creates gaps in the event ids
      allEventIdsCurrently <- unique(data$eventIndex)
      allEventIdsCurrently <- allEventIdsCurrently[!is.na(allEventIdsCurrently)]
      currentEventIdRow <- match(eventId, allEventIdsCurrently)
      previosEventId <- allEventIdsCurrently[currentEventIdRow - 1]
      nextEventId <- allEventIdsCurrently[currentEventIdRow + 1]

      # Get the last line from the previous event
      eventInfoPrevious <- data[data$eventIndex == previosEventId, ]
      eventInfoPrevious <- eventInfoPrevious[nrow(eventInfoPrevious), ]

      # And the first line of the next event
      eventInfoNext <- data[data$eventIndex == nextEventId, ]
      eventInfoNext <- eventInfoNext[1, ]

      # Check if we are between two fixations
      # Note: As we only merge fixations we ignore all other cases
      if (eventInfoPrevious$classification == "fixation" && eventInfoNext$classification == "fixation") {
        # If this is the case, calculate the angle between the fixations

        # First get the head position as average between the last point in the previous fixation and the first point in the next fixation
        origin_x <- (eventInfoPrevious$gazeOrigin_x + eventInfoNext$gazeOrigin_x) / 2
        origin_y <- (eventInfoPrevious$gazeOrigin_y + eventInfoNext$gazeOrigin_y) / 2
        origin_z <- (eventInfoPrevious$gazeOrigin_z + eventInfoNext$gazeOrigin_z) / 2

        # Calculate the direction of the gaze in the last point in the previous fixation and the first point in the next fixation
        previousDirection_x <- origin_x - eventInfoPrevious$gazePoint_x
        previousDirection_y <- origin_y - eventInfoPrevious$gazePoint_y
        previousDirection_z <- origin_z - eventInfoPrevious$gazePoint_z
        nextDirection_x <- origin_x - eventInfoNext$gazePoint_x
        nextDirection_y <- origin_y - eventInfoNext$gazePoint_y
        nextDirection_z <- origin_z - eventInfoNext$gazePoint_z

        # Calculate the scalar product of the two vectors
        scalar <- previousDirection_x * nextDirection_x + previousDirection_y * nextDirection_y + previousDirection_z * nextDirection_z

        # Calculate the length of the two vectors
        previousDirection_length <- sqrt(previousDirection_x * previousDirection_x + previousDirection_y * previousDirection_y + previousDirection_z * previousDirection_z)
        nextDirection_length <- sqrt(nextDirection_x * nextDirection_x + nextDirection_y * nextDirection_y + nextDirection_z * nextDirection_z)

        # Calculate the angle between the two vectors using the scalar product and their lengths (result in rad)
        angleRad <- acos(scalar / (previousDirection_length * nextDirection_length))

        # The final angle is supposed to be in degrees instead of rad, therefore we translate the value
        angleDeg <- (angleRad * 180) / pi

        # Now check if this angle is smaller or equal than the specified limit
        if (angleDeg <= max_angle) {
          # If all these conditions are met, merge the fixations.
          # For this we extend the previous event and merge the gap as well as the following fixation into one

          # The rows that form the new fixation event
          newEventRows <- data$eventIndex >= previosEventId & data$eventIndex <= nextEventId

          # The new fixation position is the average position of the two fixations we are merging (not including the gap)
          data$fixation_x[newEventRows] <- mean(data$gazePoint_x[newEventRows][originalEventTypes[newEventRows] != "saccade"])
          data$fixation_y[newEventRows] <- mean(data$gazePoint_y[newEventRows][originalEventTypes[newEventRows] != "saccade"])
          data$fixation_z[newEventRows] <- mean(data$gazePoint_z[newEventRows][originalEventTypes[newEventRows] != "saccade"])

          # The duration is simply the sum of the three events we are merging
          data$eventDuration[newEventRows] <- sum(unique(data$eventDuration[newEventRows]))

          # The event type is a fixation
          data$eventType[newEventRows] <- "fixation"

          # The new event index is the one of the first event
          data$eventIndex[newEventRows] <- previosEventId

          # Mark all modified rows
          data$modified[newEventRows] <- TRUE
        }
      }
    }
  }

  # Return the result
  return(data)
}
