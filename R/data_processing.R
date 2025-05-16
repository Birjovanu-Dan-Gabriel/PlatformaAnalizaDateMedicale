preprocess_health_data <- function(raw_data) {
  require(dplyr)
  
  #Verificare structure date
    required_cols <- c("ID", "Nume", "Prenume", "Varsta", "Gen", "Regiune", "Greutate", "Inaltime", "Tensiune")
  if (!all(required_cols %in% names(raw_data))) {
    stop("Structură invalidă a datelor. Coloane necesare: ", paste(required_cols, collapse = ", "))
  }
  
  #Prelucrea datelor
  clean_data <- raw_data %>%
    mutate(
      Nume_complet = paste(Nume, Prenume),
        across(c(Varsta, Greutate, Inaltime), as.numeric),
      Gen = as.character(Gen),
          Regiune = as.character(Regiune),
      Tensiune = as.character(Tensiune),
      
      # Calcul Bmi cu validare
      Bmi = case_when(
        is.na(Greutate) | is.na(Inaltime) ~ NA_real_,
        Inaltime <= 0 ~ NA_real_,
        TRUE ~ round(Greutate/((Inaltime/100)^2), 1)
      ),
      
      # Validare tensiune arteriala
      t_valida = grepl("^\\d+/\\d+$", Tensiune),
        t_sistolica = ifelse(t_valida, as.numeric(sapply(strsplit(Tensiune, "/"), `[`, 1)), NA),
        t_diastolica = ifelse(t_valida, as.numeric(sapply(strsplit(Tensiune, "/"), `[`, 2)), NA),
      
      # Crearea Grupelor de varsta
      Grup_Varsta = cut(Varsta,
                      breaks = c(0, 18, 35, 50, 65, Inf),
                      labels = c("0-18", "19-35", "36-50", "51-65", "65+"),
                      right = FALSE),
      
      # Calcul risc
      Scor_Risc = case_when(
        !is.na(Bmi) & !is.na(t_sistolica) & Bmi > 30 & t_sistolica > 140 ~ 3,
        !is.na(Bmi) & !is.na(t_sistolica) & (Bmi > 30 | t_sistolica > 140) ~ 2,
        TRUE ~ 1
      ),
      Categorie_Risc = factor(Scor_Risc, levels = 1:3, labels = c("Low", "Medium", "High"))
    ) %>%
    filter(!is.na(ID)) %>%
    arrange(desc(Scor_Risc), Varsta)
  
  return(clean_data)
}
