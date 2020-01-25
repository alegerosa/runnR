#' Pace km to pace mi
#'
#' This function converts paces from minutes per kilometer to minutes per mile
#' @param pace_km A string or vector of strings with pace in minutes per kilometer, where the minutes are separated from the seconds by a colon.
#' @return A string or vector of strings with the pace in minutes per mile, where the minutes are separated from the seconds by a colon.
#' @examples
#' pace_km_to_pace_mi("6:03")
#' pace_km_to_pace_kmi(c("7:30", "5:27", "6:42"))
#'
#' @export
pace_km_to_pace_mi <- function(pace_km) {

  if(
    !(is.character(pace_km) &&
      stringr::str_detect(pace_km, pattern = ":"))
  ) {
    stop('This function only works with the paces inputed as strings of minutes and seconds separated by a \':\', check that all your inputs meet this format: \'6:45\'')
  }

  dur_km <- lubridate::duration(
    mins = as.numeric(stringr::word(pace_km, 1, sep = stringr::fixed(":"))),
    seconds = as.numeric(stringr::word(pace_km, 2, sep = stringr::fixed(":")))
  )

  dur_mi <- dur_km * 1.609344

  pace_mi_period <- lubridate::as.period(dur_mi)

  pace_mi <- paste(
    lubridate::minute(pace_mi_period),
    ":",
    stringr::str_pad(floor(lubridate::second(pace_mi_period)), 2, "left", pad = "0"),
    sep = ""
  )
  return(pace_mi)
}

pace_km_to_pace_mi(c("2:59", NA))

#' Pace mi to pace km
#'
#' This function converts paces from minutes per mile to minutes per kilometer
#' @param pace_mi A string or vector of strings with pace in minutes per mile, where the minutes are separated from the seconds by a colon.
#' @return A string or vector of strings with the pace in minutes per kilometer, where the minutes are separated from the seconds by a colon.
#' @examples
#' pace_mi_to_pace_km("7:45")
#' pace_mi_to_pace_km(c("11:02", "9:00", "10:34"))
#'
#' @export
pace_mi_to_pace_km <- function(pace_mi) {

  if(
    !(is.character(pace_mi) &&
      stringr::str_detect(pace_mi, pattern = ":"))
  ) {
    stop('This function only works with the paces inputed as strings of minutes and seconds separated by a \':\', check that all your inputs meet this format: \'6:45\'')
  }

  dur_mi <- lubridate::duration(
    mins = as.numeric(stringr::word(pace_mi, 1, sep = stringr::fixed(":"))),
    seconds = as.numeric(stringr::word(pace_mi, 2, sep = stringr::fixed(":")))
  )

  dur_km <- dur_mi / 1.609344

  pace_km_period <- lubridate::as.period(dur_km)

  pace_km <- paste(
    lubridate::minute(pace_km_period),
    ":",
    stringr::str_pad(floor(lubridate::second(pace_km_period)), 2, "left", pad = "0"),
    sep = ""
  )
  return(pace_km)
}

pace_mi_to_pace_km(c("8:40", "4:50"))
