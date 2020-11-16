### Solution topdown-promille-ex ###

#' Computing the blood alcohol concentration, given a person's age, sex, height,
#' and weight, the drinks consumed, and taking into account the duration of the
#' consumption period
#' 
#' Based on: https://web.archive.org/web/20150123143123/http://promille-rechner.org/erlaeuterung-der-promille-berechnung/
#'
#' @param age age (in years)
#' @param sex sex ("male" or "female")
#' @param height height (in cm)
#' @param weight weight (in kg)
#' @param drinking_time start and end date and time of alcohol consumption as a
#' sorted POSIXct vector
#' @param drinks vector or list with number of consumed drinks (possible drink
#' types/names: "massn", "hoibe", "wein", "schnaps")
#'
#' @return blood alcohol concentration (in per-mille)
#' @export
tell_me_how_drunk <- function(age, sex = c("male", "female"), height, weight,
                              drinking_time, drinks) {
  # input homogenization
  sex <- tolower(sex)
  sex <- match.arg(sex)
  drinks <- unlist(as.list(drinks))

  check_input(age, sex, height, weight, drinking_time, drinks)

  alcohol_mass <- compute_alcohol_mass(drinks)

  total_body_water <- compute_total_body_water(age, sex, height, weight)

  # computing blood alcohol concentration (BAC) without taking decrease of BAC
  # over time into account
  concentration <- compute_alcohol_concentration(alcohol_mass, total_body_water)

  # correcting BAC if drinking period was more than one hour (BAC decrease starts
  # after one hour)
  drinking_duration <- as.double(difftime(drinking_time[2], drinking_time[1]),
    units = "hours"
  )
  if (drinking_duration > 1) {
    concentration <- max(0, concentration - ((drinking_duration - 1) * 0.15))
  }
  
  concentration
}

# checking inputs
check_input <- function(age, sex, height, weight, drinking_time, drinks) {
  checkmate::assert_number(age, lower = 0, finite = TRUE)
  checkmate::assert_number(height, lower = 0, finite = TRUE)
  checkmate::assert_number(weight, lower = 0, finite = TRUE)
  checkmate::assert_posixct(drinking_time,
    any.missing = FALSE, all.missing = FALSE, len = 2, sorted = TRUE
  )
  checkmate::assert_numeric(drinks,
    lower = 0, finite = TRUE, any.missing = FALSE, all.missing = FALSE,
    names = "named"
  )
  checkmate::assert_subset(names(drinks), choices = c("massn", "hoibe", "wein", "schnaps"))
  check_legal_drinking_age(age, drinks)
}

# checking if entered values are illegal combinations
check_legal_drinking_age <- function(age, drinks) {
  # anything under age < 16 not ok, hard liquor under age 18 not ok
  if ((age < 16 && drinks > 0) ||
    (age < 18 && isTRUE(drinks["schnaps"] > 0))) { # isTRUE because of NA if no "schnaps" was entered
    warning("Entered age and/or drinks lie in illegal territory.")
  }
}

# computing alcohol mass in grams
# assumed volumes and Vol-%:
#   "massn": 1000ml and 6%
#   "hoibe": 500ml and 6%
#   "wein": 200ml and 11%
#   "schnaps": 40ml and 40%
compute_alcohol_mass <- function(drinks) {
  # creating variables "massn_counts", "hoibe_count", "wein_count" and
  # "schnaps_count" with the total number for each drink type
  for (i in c("massn", "hoibe", "wein", "schnaps")) {
    assign(paste0(i, "_count"), sum(drinks[names(drinks) == i]))
  }
  # volume in ml
  beer_volume <- massn_count * 1000 + hoibe_count * 500
  wein_volume <- wein_count * 200
  schnaps_volume <- schnaps_count * 40
  # alcohol mass in grams
  (beer_volume * 0.06 + wein_volume * 0.11 + schnaps_volume * 0.4) * 0.8
}

# computing total body water
compute_total_body_water <- function(age, sex, height, weight) {
  switch(sex,
    "male" = 2.447 - 0.09516 * age + 0.1074 * height + 0.3362 * weight,
    "female" = 0.203 - 0.07 * age + 0.1069 * height + 0.2466 * weight
  )
}

# computing blood alcohol concentration in per-mille
compute_alcohol_concentration <- function(alcohol_mass, total_body_water) {
  concentration <- (0.8 * alcohol_mass) / (1.055 * total_body_water)
}

testthat::test_file("topdown-promille-tests.R")
