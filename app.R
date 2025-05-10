library(shiny)
library(shinymanager)
library(digest)
library(DT)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(shinythemes)


credentials <- data.frame(
  user = c("pacient", "doctor"),
  password = sapply(c("parola1", "parola2"), digest, algo = "sha256", serialize = FALSE),
  role = c("user", "admin"),
  stringsAsFactors = FALSE
)

source("R/data_processing.R") 


ui <- secure_app(
  fluidPage(
    theme = shinythemes::shinytheme("flatly"),  # Opțional: temă predefinită
    titlePanel("Platformă Analiză Date Sănătate"),
    uiOutput("main_ui")
  )
)


server <- function(input, output, session) {

  res_auth <- secure_server(check_credentials = check_credentials_custom(credentials))
  
  
  raw_data <- reactiveVal()
  processed_data <- reactiveVal()
 
  observe({
  
    if (file.exists("data/raw_data.csv")) {
      raw_data(read.csv("data/raw_data.csv", stringsAsFactors = FALSE))
    } else {
      raw_data(data.frame(
        patient_id = integer(),
        age = numeric(),
        gender = character(),
        region = character(),
        weight_kg = numeric(),
        height_cm = numeric(),
        bmi = numeric(),
        blood_pressure = character(),
        stringsAsFactors = FALSE
      ))
    }
    
    if (nrow(raw_data()) > 0) {
      processed_data(preprocess_health_data(raw_data()))
      write.csv(processed_data(), "data/processed_data.csv", row.names = FALSE)
    }
  })
  
  output$main_ui <- renderUI({
    req(res_auth$user)
    
    if (res_auth$role == "admin") {
      tabsetPanel(
        tabPanel("Date Brute", 
                 DTOutput("raw_data_table"),
                 tags$hr(),
                 actionButton("process_data", "Procesează Date")),
        
        tabPanel("Date Prelucrate", 
                 DTOutput("processed_data_table"),
                 plotOutput("age_distribution")),
        
        tabPanel("Administrare",
                 fluidRow(
                   column(6,
                          h4("Adăugare Date"),
                          numericInput("new_patient_id", "ID Pacient", value = max(raw_data()$patient_id, na.rm = TRUE) + 1),
                          numericInput("new_age", "Vârstă", value = 30),
                          selectInput("new_gender", "Gen", c("M", "F")),
                          textInput("new_region", "Regiune", value = "Urban"),
                          numericInput("new_weight", "Greutate (kg)", value = 70),
                          numericInput("new_height", "Înălțime (cm)", value = 170),
                          textInput("new_bp", "Tensiune arterială (format: 120/80)", value = "120/80"),
                          actionButton("add_patient", "Adaugă Pacient", class = "btn-primary")
                   ),
                   column(6,
                          h4("Ștergere Date"),
                          selectizeInput("delete_patient", "Selectează Pacient", choices = NULL),
                          actionButton("delete_patient_btn", "Șterge Pacient", class = "btn-danger")
                   )
                 ))
      )
    } else {
      tabsetPanel(
        tabPanel("Dashboard",
                 fluidRow(
                   valueBoxOutput("total_patients"),
                   valueBoxOutput("avg_age"),
                   valueBoxOutput("high_risk")
                 ),
                 plotOutput("risk_plot")),
        tabPanel("Listă Pacienți",
                 DTOutput("patient_list"))
      )
    }
  })
  
  output$raw_data_table <- renderDT({
    datatable(raw_data(), 
              editable = TRUE,
              options = list(scrollX = TRUE))
  })
  
  output$processed_data_table <- renderDT({
    datatable(processed_data(),
              options = list(scrollX = TRUE))
  })
  
  output$age_distribution <- renderPlot({
    req(processed_data())
    ggplot(processed_data(), aes(x = age_group, fill = gender)) +
      geom_bar() +
      labs(title = "Distribuție pe Vârste", x = "Grup de Vârstă", y = "Număr Pacienți")
  })
  
  # Gestionare evenimente
  observeEvent(input$process_data, {
    processed_data(preprocess_health_data(raw_data()))
    write.csv(processed_data(), "data/processed_data.csv", row.names = FALSE)
    showNotification("Date procesate cu succes!", type = "message")
  })
  
  observeEvent(input$raw_data_table_cell_edit, {
    info <- input$raw_data_table_cell_edit
    df <- raw_data()
    df[info$row, info$col] <- info$value
    raw_data(df)
    write.csv(df, "data/raw_data.csv", row.names = FALSE)
  })
  
  
  observe({
    updateSelectizeInput(session, "delete_patient", 
                         choices = raw_data()$patient_id,
                         server = TRUE)
  })

  
  observeEvent(input$add_patient, {
    # Validare date
    req(input$new_patient_id, input$new_age, input$new_gender)
    
    new_patient <- data.frame(
      patient_id = as.numeric(input$new_patient_id),
      age = as.numeric(input$new_age),
      gender = as.character(input$new_gender),
      region = as.character(input$new_region),
      weight_kg = as.numeric(input$new_weight),
      height_cm = as.numeric(input$new_height),
      bmi = round(input$new_weight/((input$new_height/100)^2), 1),
      blood_pressure = as.character(input$new_bp),
      stringsAsFactors = FALSE
    )
    
    # Verificare format tensiune arterială
    if (!grepl("^\\d+/\\d+$", new_patient$blood_pressure)) {
      showNotification("Format tensiune invalid! Folosește formatul: 120/80", type = "error")
      return()
    }
    
    # Actualizare date
    tryCatch({
      updated_data <- bind_rows(raw_data(), new_patient)
      raw_data(updated_data)
      write.csv(updated_data, "data/raw_data.csv", row.names = FALSE)
      
      # Resetare formular
      updateNumericInput(session, "new_patient_id", value = max(updated_data$patient_id, na.rm = TRUE) + 1)
      updateNumericInput(session, "new_age", value = 30)
      updateSelectInput(session, "new_gender", selected = "M")
      updateTextInput(session, "new_region", value = "Urban")
      updateNumericInput(session, "new_weight", value = 70)
      updateNumericInput(session, "new_height", value = 170)
      updateTextInput(session, "new_bp", value = "120/80")
      
      showNotification("Pacient adăugat cu succes!", type = "message")
    }, error = function(e) {
      showNotification(paste("Eroare la adăugare:", e$message), type = "error")
    })
  })

  
  observeEvent(input$delete_patient_btn, {
    req(input$delete_patient)
    updated_data <- raw_data()[raw_data()$patient_id != input$delete_patient, ]
    raw_data(updated_data)
    write.csv(updated_data, "data/raw_data.csv", row.names = FALSE)
  })

  
  output$total_patients <- renderValueBox({
    valueBox(
      nrow(processed_data()), 
      "Total Pacienți", 
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$avg_age <- renderValueBox({
    valueBox(
      round(mean(processed_data()$age, na.rm = TRUE)),  # Corectat - mutat paranteza
      "Vârstă Medie", 
      icon = icon("calendar"),
      color = "green"
    )
  })
    
    output$high_risk <- renderValueBox({
      high_risk_count <- sum(processed_data()$risk_score >= 2, na.rm = TRUE)
      valueBox(
        high_risk_count, 
        "Pacienți Risc Ridicat", 
        icon = icon("exclamation-triangle"),
        color = ifelse(high_risk_count > 5, "red", "yellow")
      )
    })
    
    output$risk_plot <- renderPlot({
      ggplot(processed_data(), aes(x = age, y = risk_score, color = gender)) +
        geom_point(size = 3) +
        geom_smooth(method = "lm") +
        labs(title = "Scor de Risc pe Vârstă", x = "Vârstă", y = "Scor Risc")
    })
    
    output$patient_list <- renderDT({
      datatable(processed_data() %>% 
                  select(patient_id, age, gender, region, risk_score),
                options = list(pageLength = 5))
    })
}

shinyApp(ui, server)