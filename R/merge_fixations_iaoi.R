# Copyright (c) Sebastian Kapp.
# Licensed under the MIT License.

#' Merge Fixations from I-AOI Classifier
#'
#' @description
#' Merge fixations classified by \code{\link[arett]{classify_iaoi}} into a new, combined fixation when they are on the same AOI and closer to each other than the max time specified.
#'
#' @details
#' This is a custom implementation for merging fixations on the same AOI with a gap smaller than the specified time between them.
#'
#' @format Input data frame columns
#' \describe{
#'   \item{gazePoint_x}{X coordinates of the gaze point}
#'   \item{gazePoint_y}{Y coordinates of the gaze point}
#'   \item{gazePoint_z}{Z coordinates of the gaze point}
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
#' @return The input data frame with the fixations merged
#'
#' @export
merge_fixations_iaoi <- function(data, max_time = 75) {
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
        # If this is the case, check if the fixations are of the same AOI (due to the I-AOI filter this is the differentiation between fixations)
        if (eventInfoPrevious$gazePointAOI_name == eventInfoNext$gazePointAOI_name) {
          # If all these conditions are met, merge the fixations.
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

  return(data)
}
