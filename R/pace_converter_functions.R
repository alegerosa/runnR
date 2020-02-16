#' Pace km to pace mi, text
#'
#' This function converts paces from minutes per kilometer to minutes per mile, taking strings as inputs and returning strings as outputs
#' @param pace_km_text A string or vector of strings with pace in minutes per kilometer, where the minutes are separated from the seconds by a colon.
#' @return A string or vector of strings with the pace in minutes per mile, where the minutes are separated from the seconds by a colon.
#' @examples
#' pace_km_to_pace_mi_text("6:03")
#' pace_km_to_pace_mi_text(c("7:30", "5:27", "6:42"))
#'
#' @export
pace_km_to_mi_text <- function(pace_km_text) {

  if(
    !(is.character(pace_km_text) &&
      stringr::str_detect(pace_km_text, pattern = ":"))
  ) {
    stop('This function only works with the paces inputed as strings of minutes and seconds separated by a \':\', check that all your inputs meet this format: \'6:45\'')
  }

  dur_km <- lubridate::duration(
    mins = as.numeric(stringr::word(pace_km_text, 1, sep = stringr::fixed(":"))),
    seconds = as.numeric(stringr::word(pace_km_text, 2, sep = stringr::fixed(":")))
  )

  dur_mi <- dur_km * 1.609344

  pace_mi_period <- lubridate::as.period(dur_mi)

  pace_mi_text <- paste(
    lubridate::minute(pace_mi_period),
    ":",
    stringr::str_pad(floor(lubridate::second(pace_mi_period)), 2, "left", pad = "0"),
    sep = ""
  )
  return(pace_mi_text)
}

pace_km_to_mi_text(c("2:59", NA))

#' Pace mi to pace km, text
#'
#' This function converts paces from minutes per mile to minutes per kilometer, taking strings as inputs and returning strings as outputs
#' @param pace_mi_text A string or vector of strings with pace in minutes per mile, where the minutes are separated from the seconds by a colon.
#' @return A string or vector of strings with the pace in minutes per kilometer, where the minutes are separated from the seconds by a colon.
#' @examples
#' pace_mi_to_km_text("7:45")
#' pace_mi_to_km_text(c("11:02", "9:00", "10:34"))
#'
#' @export
pace_mi_to_km_text <- function(pace_mi_text) {

  if(
    !(is.character(pace_mi_text) &&
      stringr::str_detect(pace_mi_text, pattern = ":"))
  ) {
    stop('This function only works with the paces inputed as strings of minutes and seconds separated by a \':\', check that all your inputs meet this format: \'6:45\'')
  }

  dur_mi <- lubridate::duration(
    mins = as.numeric(stringr::word(pace_mi_text, 1, sep = stringr::fixed(":"))),
    seconds = as.numeric(stringr::word(pace_mi_text, 2, sep = stringr::fixed(":")))
  )

  dur_km <- dur_mi / 1.609344

  pace_km_period <- lubridate::as.period(dur_km)

  pace_km_text <- paste(
    lubridate::minute(pace_km_period),
    ":",
    stringr::str_pad(floor(lubridate::second(pace_km_period)), 2, "left", pad = "0"),
    sep = ""
  )
  return(pace_km_text)
}

pace_mi_to_km_text(c("8:40", "4:50"))

#' Pace km to pace mi, number input
#'
#' This function converts a pace from minutes per kilometer to minutes per mile, taking two numbers as inputs (minutes and seconds) and returning a string as output. Only works with one observation at a time for now.
#' @param pace_km_min A number with the whole minutes in the pace in minute per kilometer
#' @param pace_km_sec A number with the seconds in the pace in minute per kilometer
#' @return A string with the pace in minutes per mile, where the minutes are separated from the seconds by a colon.
#' @examples
#' pace_km_to_mi_num_input(7,30)
#' pace_km_to_mi_num_input(5,27)
#'
#' @export
pace_km_to_mi_num_input <- function(pace_km_min, pace_km_sec) {

  dur_km <- lubridate::duration(
    mins = pace_km_min,
    seconds = pace_km_sec
  )

  dur_mi <- dur_km * 1.609344

  pace_mi_period <- lubridate::as.period(dur_mi)

  pace_mi_text <- paste(
    lubridate::minute(pace_mi_period),
    ":",
    stringr::str_pad(floor(lubridate::second(pace_mi_period)), 2, "left", pad = "0"),
    sep = ""
  )
  return(pace_mi_text)
}

pace_km_to_mi_num_input(7,30)
pace_km_to_mi_num_input(5,27)

#' Pace mi to pace km, number input
#'
#' This function converts a pace from minutes per mile to minutes per kilometer, taking two numbers as inputs (minutes and seconds) and returning a string as output. Only works with one observation at a time for now.
#' @param pace_mi_min A number with the whole minutes in the pace in minute per mi
#' @param pace_mi_sec A number with the seconds in the pace in minute per mi
#' @return A string with the pace in minutes per kilometer, where the minutes are separated from the seconds by a colon.
#' @examples
#' pace_mi_to_km_num_input(7,30)
#' pace_mi_to_km_num_input(5,27)
#'
#' @export
pace_mi_to_km_num_input <- function(pace_mi_min, pace_mi_sec) {

  dur_mi <- lubridate::duration(
    mins = pace_mi_min,
    seconds = pace_mi_sec
  )

  dur_km <- dur_mi / 1.609344

  pace_km_period <- lubridate::as.period(dur_km)

  pace_km_text <- paste(
    lubridate::minute(pace_km_period),
    ":",
    stringr::str_pad(floor(lubridate::second(pace_km_period)), 2, "left", pad = "0"),
    sep = ""
  )
  return(pace_km_text)
}

pace_mi_to_km_num_input(7,30)
pace_mi_to_km_num_input(5,27)
