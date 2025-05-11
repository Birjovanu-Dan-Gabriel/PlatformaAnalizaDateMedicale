preprocess_health_data <- function(raw_data) {
  require(dplyr)
  
  #Verificare structure date
  required_cols <- c("patient_id", "nume", "prenume", "age", "gender", "region", "weight_kg", "height_cm", "blood_pressure")
  if (!all(required_cols %in% names(raw_data))) {
    stop("Structură invalidă a datelor. Coloane necesare: ", paste(required_cols, collapse = ", "))
  }
  
  #Prelucrea datelor
  clean_data <- raw_data %>%
    mutate(
      nume_complet = paste(nume, prenume),
      across(c(age, weight_kg, height_cm), as.numeric),
      gender = as.character(gender),
      region = as.character(region),
      blood_pressure = as.character(blood_pressure),
      
      # Calcul BMI cu validare
      bmi = case_when(
        is.na(weight_kg) | is.na(height_cm) ~ NA_real_,
        height_cm <= 0 ~ NA_real_,
        TRUE ~ round(weight_kg/((height_cm/100)^2), 1)
      ),
      
      # Validare tensiune arteriala
      bp_valid = grepl("^\\d+/\\d+$", blood_pressure),
      bp_systolic = ifelse(bp_valid, as.numeric(sapply(strsplit(blood_pressure, "/"), `[`, 1)), NA),
      bp_diastolic = ifelse(bp_valid, as.numeric(sapply(strsplit(blood_pressure, "/"), `[`, 2)), NA),
      
      # Crearea Grupelor de varsta
      age_group = cut(age,
                      breaks = c(0, 18, 35, 50, 65, Inf),
                      labels = c("0-18", "19-35", "36-50", "51-65", "65+"),
                      right = FALSE),
      
      # Calcul risc
      risk_score = case_when(
        !is.na(bmi) & !is.na(bp_systolic) & bmi > 30 & bp_systolic > 140 ~ 3,
        !is.na(bmi) & !is.na(bp_systolic) & (bmi > 30 | bp_systolic > 140) ~ 2,
        TRUE ~ 1
      ),
      risk_category = factor(risk_score, levels = 1:3, labels = c("Low", "Medium", "High"))
    ) %>%
    filter(!is.na(patient_id)) %>%
    arrange(desc(risk_score), age)
  
  return(clean_data)
}
