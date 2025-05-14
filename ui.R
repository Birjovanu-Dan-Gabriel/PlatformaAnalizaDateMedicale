# ui.R

# Function for creating login credentials
credentials <- data.frame(
  user = c("", "doctor"),
  password = sapply(c("parola1", "parola2"), digest, algo = "sha256", serialize = FALSE),
  role = c("user", "admin"),
  stringsAsFactors = FALSE
)

# Data processing
source("R/data_processing.R")

# Adăugăm ID-uri unice pentru elemente critice
uiOutput("main_ui") %>% 
  tagAppendAttributes(id = "main_panel")

# Butonul de căutare trebuie să aibă ID constant
actionButton("patient_search_btn", "Caută", 
             id = "patient_search_btn",
             class = "btn-primary shiny-bound-input")

# UI definition
ui <- secure_app(
  fluidPage(
    theme = shinythemes::shinytheme("flatly"),  
    titlePanel("Platformă Analiză Date Sănătate"),
    uiOutput("main_ui")
  )
)