### Draft/Notizen für topdown-promille-ex ###
library(checkmate)

tell_me_how_drunk <- function(age, sex = c("male", "female"), height, weight, drinking_time, drinks) {
  check_input(age, sex, height, weight, drinking_time, drinks)
  
  sex <- tolower(sex)
  sex <- match.arg(sex)
  drinks <- unlist(as.list(drinks))
  print(drinks)
  
  alcohol_mass <- compute_alcohol_mass(drinks)
  print(alcohol_mass)
  
  total_body_water <- compute_total_body_water(age, sex, height, weight)
  print(total_body_water)
  
  concentration <- compute_alcohol_concentration(alcohol_mass, total_body_water)
  
  drinking_duration <- as.double(difftime(drinking_time[2], drinking_time[1]), units = "hours")
  print(drinking_duration)
  print(concentration)
  if (drinking_duration > 1) {
    concentration <- max(0, concentration - ((drinking_duration - 1) * 0.15))
  }
  concentration
}  # hier noch anpassen, dass Abbau nach 1 Stunde beginnt

check_input <- function(age, sex, height, weight, drinking_time, drinks) {
  checkmate::assert_number(age, lower = 0, finite = TRUE)
  checkmate::assert_string(sex)
  checkmate::assert_number(height, lower = 0, finite = TRUE)
  checkmate::assert_number(weight, lower = 0, finite = TRUE)
  checkmate::assert_posixct(drinking_time,
    any.missing = FALSE, all.missing = FALSE, len = 2, sorted = TRUE
  )
  checkmate::assert(check_numeric(drinks), check_list(drinks), combine = "or")
  checkmate::assert_numeric(unlist(drinks),
    lower = 0, finite = TRUE, any.missing = FALSE, all.missing = FALSE,
    names = "named"
  )
  checkmate::assert_subset(names(unlist(drinks)), choices = c("massn", "hoibe", "wein", "schnaps"))
  check_legal_drinking_age(age, drinks)
}

check_legal_drinking_age <- function(age, drinks) {
  if (age < 16 || (age < 18 && any(names(unlist(drinks)) == "schnaps"))) {
    warning("Entered age or drinks lie in illegal range.")
  }
} # hier noch if Bedingung anpassen

# compute alcohol mass in grams
# inputs: ...
# output: numeric value
compute_alcohol_mass <- function(drinks) {
  for (i in c("massn", "hoibe", "wein", "schnaps")) {
    assign(paste0(i, "_count"), sum(drinks[names(drinks) == i]))
  }
  print(c(massn_count, hoibe_count, wein_count, schnaps_count))
  # volume in ml
  beer_volume <- massn_count * 1000 + hoibe_count * 500
  wein_volume <- wein_count * 200
  schnaps_volume <- schnaps_count * 40
  (beer_volume * 0.06 + wein_volume * 0.11 + schnaps_volume * 0.4) * 0.8
}

compute_total_body_water <- function(age, sex, height, weight) {
  switch(sex,
    "male" = 2.447 - 0.09516 * age + 0.1074 * height + 0.3362 * weight,
    "female" = 0.203 - 0.07 * age + 0.1069 * height + 0.2466 * weight
  )
}

compute_alcohol_concentration <- function(alcohol_mass, total_body_water) {
  concentration <- (0.8 * alcohol_mass) / (1.055 * total_body_water)
}
