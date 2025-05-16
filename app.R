# app.R
library(shiny)
library(shinymanager)
library(digest)
library(DT)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(shinythemes)
library(htmltools)  # pentru export PDF
library(webshot)    # pentru export PDF
library(gridExtra)

# incarca componenta ui.R
source("ui.R")


# functia pentru crearea PDF-ului
export_plot_to_pdf <- function(plot, filename) {
  temp_plot <- tempfile(fileext = ".png")
  ggsave(temp_plot, plot = plot, device = "png", width = 15, height = 6)
  pdf_file <- paste0(tempfile(), ".pdf")
  
  # aici il este creat pdful din plot
  pdf(pdf_file, width = 15, height = 6)
  print(plot)
  dev.off()
  
  return(pdf_file)
}

#functia de exportare a tabelei in pdf
export_table_to_pdf <- function(data, filename, title = "") {
  pdf_file <- paste0(tempfile(), ".pdf")
  
  pdf(pdf_file, paper = "a4r", width = 11, height = 8)
  gridExtra::grid.table(data)
  if (title != "") {
    grid::grid.text(title, y = 0.95, gp = grid::gpar(fontsize = 14))
  }
  dev.off()
  
  return(pdf_file)
}


#inceput functie SERVER
server <- function(input, output, session) {
  #verificarea credientialelor
  res_auth <- secure_server(check_credentials = check_credentials_custom(credentials))
  raw_data <- reactiveVal()
  processed_data <- reactiveVal()
  
  observe({
    #verificam daca exista fisierul raw_data.csv 
    #in cazul in care acest fisier nu exista declaram variabilele
    if (file.exists("data/raw_data.csv")) {
      raw_data(read.csv("data/raw_data.csv", stringsAsFactors = FALSE))
    } else {
      raw_data(data.frame(
        ID = integer(),
        Nume = character(),
        Prenume = character(),
        Varsta = numeric(),
        Gen = character(),
        Regiune = character(),
        Greutate = numeric(),
        Inaltime = numeric(),
        Bmi = numeric(),
        Tensiune = character(),
        Colesterol = character(),
        Glucoza = numeric(),
        Fumator = character(),
        Diabetic = character(),
        Activitate = character(),
        stringsAsFactors = FALSE
      ))
    }
    
    #procesam datele daca acestea exista
    if (nrow(raw_data()) > 0) {
      processed_data(preprocess_health_data(raw_data()))
      write.csv(processed_data(), "data/processed_data.csv", row.names = FALSE)
    }
  })
  
  #render UI
  output$main_ui <- renderUI({
    req(res_auth$user)
    
    # UI-ul pentru admini
    if (res_auth$role == "admin") {
      tabsetPanel(
        # pag1 - contine date brute date brute
        tabPanel("Date Brute", 
                 DTOutput("raw_data_table"),
                 textInput("search_patient", "Cauta pacient (Nume, Prenume sau ID)"),
                 actionButton("search_btn", "Cauta"),
                 tags$hr(),
                 # Programul proceseaza datele automat la lansare si la adaugarea/stergerea a noi intrari
                 # Cu toate acestea am adaugat acest buton in cazul in care procedeul implementat de procesare automata esueaza
                 actionButton("process_data", "Proceseaza Date"),
                 downloadButton("export_raw_pdf", "Export PDF")),
        
        # pag2 - contine datele prelucrate
        tabPanel("Date Prelucrate", 
                 DTOutput("processed_data_table"),
                 plotOutput("age_distribution"),
                 downloadButton("export_processed_pdf", "Export PDF")),  
        
        # pag3 - contine niste grafice si statistici
        tabPanel("Statistici",
                 fluidRow(
                   column(6, plotOutput("age_risk_plot")),
                   column(6, plotOutput("condition_distribution_plot"))
                 ),
                 fluidRow(
                   valueBoxOutput("avg_glucose_box", width = 6), 
                   valueBoxOutput("high_bp_box", width = 6)
                 ),
                 downloadButton("export_stats_pdf", "Export PDF")
        ),
        
        # pag4 - administrare - pentru adaugare/stergere pacienti
        tabPanel("Administrare",
                 fluidRow(
                   column(6,
                          #adaugarea pacientului
                          h4("Adaugare Date"),
                          numericInput("new_ID", "ID Pacient", value = max(raw_data()$ID, na.rm = TRUE) + 1),
                          textInput("new_nume", "Nume", value = ""),
                          textInput("new_Prenume", "Prenume", value = ""),
                          numericInput("new_age", "Varsta", value = 30),
                          selectInput("new_gender", "Gen", c("M", "F")),
                          textInput("new_region", "Regiune", value = "Urban"),
                          numericInput("new_weight", "Greutate (kg)", value = 70),
                          numericInput("new_height", "Inaltime (cm)", value = 170),
                          textInput("new_bp", "Tensiune arteriala (format: 120/80)", value = "120/80"),
                          selectInput("new_cholesterol", "Colesterol", 
                                      choices = c("Normal", "Borderline", "High"), selected = "Normal"),
                          numericInput("new_glucose", "Glucoza (mg/dL)", value = 100),
                          selectInput("new_smoker", "Fumator", choices = c("Yes", "No"), selected = "No"),
                          selectInput("new_diabetic", "Diabetic", choices = c("Yes", "No"), selected = "No"),
                          selectInput("new_activity", "Activitate fizica", 
                                      choices = c("Low", "Moderate", "High"), selected = "Moderate"),
                          actionButton("add_patient", "Adauga Pacient", class = "btn-primary")
                   ),
                   column(6,
                          # stergerea pacientului
                          h4("Stergere Date"),
                          selectizeInput("delete_patient", "Selectează Pacient", 
                                         choices = NULL,
                                         options = list(
                                           placeholder = 'Caută dupa Nume',
                                           maxOptions = 100
                                         )),
                          actionButton("delete_patient_btn", "Sterge Pacient", class = "btn-danger")
                   )
                 ))
      )
    } else {
      # UI-ul pentru asistena
      tabsetPanel(
        # pag1 - dashboard - cateva informatii generale
        tabPanel("Dashboard",
                 fluidRow(
                   valueBoxOutput("total_patients", width = 4),
                   valueBoxOutput("avg_age", width = 4),
                   valueBoxOutput("high_risk", width = 4)
                 ),
                 plotOutput("risk_plot")),
        
        # pag2 - statistici - ca la admin
        tabPanel("Statistici",
                 fluidRow(
                   column(6, plotOutput("age_risk_plot")),
                   column(6, plotOutput("condition_distribution_plot"))
                 ),
                 fluidRow(
                   valueBoxOutput("avg_glucose_box", width = 6),
                   valueBoxOutput("high_bp_box", width = 6)
                 ),
                 downloadButton("export_stats_pdf", "Export PDF")
        ),
        
        # pag3 - lista pacientilor
        tabPanel("Lista Pacienti",
                 fluidRow(
                   column(6,
                          textInput("patient_search_term", "Cauta pacient (Nume, Prenume sau ID)"),
                          actionButton("patient_search_btn", "Cauta") 
                   )
                 ),
                 DTOutput("patient_list")
        )
      )
    }
  })
  
  # Display raw_data
  output$raw_data_table <- renderDT({
    datatable(raw_data(), 
              editable = TRUE,
              options = list(scrollX = TRUE))
  })
  
  # Display processed_data 
  output$processed_data_table <- renderDT({
    datatable(processed_data(),
              options = list(scrollX = TRUE))
  })
  
  # Display pt graficul cu distributia pe varsta
  output$age_distribution <- renderPlot({
    req(processed_data())
    ggplot(processed_data(), aes(x = Grup_Varsta, fill = Gen)) +
      geom_bar() +
      labs(title = "Distributie pe Varste", x = "Grup de Varsta", y = "Numar Pacienți")
  })
  
  #Display grafic (ala cu patratele) corelatie vasta-risc
  output$age_risk_plot <- renderPlot({
    req(processed_data())
    ggplot(processed_data(), aes(x = Varsta, y = risk_score, color = Gen)) +
      geom_point(alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Corelație Vârstă - Scor de Risc",
           x = "Varsta", y = "Scor de Risc") +
      scale_color_manual(values = c("M" = "blue", "F" = "pink")) +
      theme_minimal()
  })
  
  # Display grafic conditii medicale
  output$condition_distribution_plot <- renderPlot({
    req(processed_data())
    processed_data() %>%
      count(Diabetic, Fumator, name = "count") %>%
      ggplot(aes(x = Diabetic, y = Fumator, fill = count)) +
      geom_tile() +
      geom_text(aes(label = count), color = "white") +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      labs(title = "Distribuție Condiții Medicale",
           x = "Diabetic", y = "Fumator") +
      theme_minimal()
  })
  
  # ValueBox cu glicemia medie
  output$avg_glucose_box <- renderValueBox({
    valueBox(
      round(mean(processed_data()$Glucoza, na.rm = TRUE)),
      "Glicemie Medie (mg/dL)", 
      icon = icon("vial"),
      color = "purple"
    )
  })
  
  # Valuebox cu nr cu tunesiunea mare
  output$high_bp_box <- renderValueBox({
    high_bp_count <- processed_data() %>%
      filter(bp_systolic > 140 | bp_diastolic > 90) %>%
      nrow()
    
    valueBox(
      high_bp_count,
      "Pacienti cu Hipertensiune", 
      icon = icon("heart-pulse"),
      color = "orange"
    )
  })
  
  output$export_processed_pdf <- downloadHandler(
    filename = function() {
      paste("date-prelucrate-", Sys.Date(), ".pdf", sep="")
    },
    content = function(file) {
      # Create the PDF directly
      pdf(file, paper = "a4", width = 8.27, height = 11.69)
      
      data <- raw_data()
      mid_col <- ceiling(ncol(data)/2) + 1
      
      # Table 1
      grid::grid.text("Date Prelucrate - Partea 1", y = 0.95, 
                      gp = grid::gpar(fontsize = 14))
      gridExtra::grid.table(data[, 1:mid_col])
      
      # Table 2
      grid::grid.newpage()
      grid::grid.text("Date Prelucrate - Partea 2", y = 0.95, 
                      gp = grid::gpar(fontsize = 14))
      gridExtra::grid.table(data[, c(1:2, (mid_col+1):ncol(data))])
      
      dev.off()
      flush.console()
    }
  )
  
  
  output$export_raw_pdf <- downloadHandler(
    filename = function() {
      paste("date-brute-", Sys.Date(), ".pdf", sep="")
    },
    content = function(file) {
      # Create the PDF directly
      pdf(file, paper = "a4", width = 8.27, height = 11.69)
      
      data <- raw_data()
      mid_col <- ceiling(ncol(data)/2)
      
      # Table 1
      grid::grid.text("Date Brute - Partea 1", y = 0.95, 
                      gp = grid::gpar(fontsize = 14))
      gridExtra::grid.table(data[, 1:mid_col])
      
      # Table 2
      grid::grid.newpage()
      grid::grid.text("Date Brute - Partea 2", y = 0.95, 
                      gp = grid::gpar(fontsize = 14))
      gridExtra::grid.table(data[, c(1:2, (mid_col+1):ncol(data))])
      
      dev.off()
      flush.console()
    }
  )
  
  output$export_stats_pdf <- downloadHandler(
    filename = function() {
      paste("statistici-", Sys.Date(), ".pdf", sep="")
    },
    content = function(file) {
      # Create a temporary directory
      temp_dir <- tempdir()
      plot_files <- c()
      
      # Save each plot
      age_risk_plot <- ggplot(processed_data(), aes(x = Varsta, y = risk_score, color = Gen)) +
        geom_point(alpha = 0.7) +
        geom_smooth(method = "lm", se = FALSE) +
        labs(title = "Corelație Vârstă - Scor de Risc",
             x = "Varsta", y = "Scor de Risc") +
        scale_color_manual(values = c("M" = "blue", "F" = "pink")) +
        theme_minimal()
      
      condition_plot <- processed_data() %>%
        count(Diabetic, Fumator, name = "count") %>%
        ggplot(aes(x = Diabetic, y = Fumator, fill = count)) +
        geom_tile() +
        geom_text(aes(label = count), color = "white") +
        scale_fill_gradient(low = "lightblue", high = "darkblue") +
        labs(title = "Distribuție Condiții Medicale",
             x = "Diabetic", y = "Fumator") +
        theme_minimal()
      
      # Create PDF
      pdf(file, width = 11, height = 8)
      print(age_risk_plot)
      print(condition_plot)
      dev.off()
    }
  )
  
  # Event handlers
  
  # Process data
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
    # Create ordered list of full names
    patient_choices <- raw_data() %>%
      mutate(full_name = paste(Nume, Prenume, " (ID:", ID, ")")) %>%
      arrange(Nume, Prenume) %>%
      select(full_name, ID)
    
    # Update dropdown with names and IDs
    updateSelectizeInput(
      session, 
      "delete_patient",
      choices = setNames(patient_choices$ID, patient_choices$full_name),
      server = TRUE
    )
  })
  
  # Search function
  observeEvent(input$search_btn, {
    search_term <- tolower(input$search_patient)
    filtered <- raw_data() %>%
      mutate(search_field = tolower(paste(Nume, Prenume, ID))) %>%
      filter(grepl(search_term, search_field))
    
    output$raw_data_table <- renderDT({
      datatable(filtered, editable = TRUE, options = list(scrollX = TRUE))
    })
  })
  
  # Search function for patient
  observeEvent(input$patient_search_btn, {
    search_term <- tolower(input$patient_search_term)
    filtered <- processed_data() %>%
      mutate(search_field = tolower(paste(Nume, Prenume, ID))) %>%
      filter(grepl(search_term, search_field))
    
    output$patient_list <- renderDT({
      datatable(filtered %>% select(Nume, Prenume, ID, Varsta, Gen, risk_score),
                options = list(pageLength = 5))
    })
  })
  
  # Add patient function
  observeEvent(input$add_patient, {
    req(input$new_ID, input$new_age, input$new_gender)
    
    new_patient <- data.frame(
      ID = as.numeric(input$new_ID),
      Nume = as.character(input$new_nume),
      Prenume = as.character(input$new_Prenume),
      Varsta = as.numeric(input$new_age),
      Gen = as.character(input$new_gender),
      Regiune = as.character(input$new_region),
      Greutate = as.numeric(input$new_weight),
      Inaltime = as.numeric(input$new_height),
      Bmi = round(input$new_weight/((input$new_height/100)^2), 1),
      Tensiune = as.character(input$new_bp),
      Colesterol = as.character(input$new_cholesterol),
      Glucoza = as.numeric(input$new_glucose),
      Fumator = as.character(input$new_smoker),
      Diabetic = as.character(input$new_diabetic),
      Activitate = as.character(input$new_activity),
      stringsAsFactors = FALSE
    )
    
    # Blood pressure format validation
    if (!grepl("^\\d+/\\d+$", new_patient$Tensiune)) {
      showNotification("Format tensiune invalid! Folosește formatul: 120/80", type = "error")
      return()
    }
    
    # Update data
    tryCatch({
      updated_data <- bind_rows(raw_data(), new_patient)
      raw_data(updated_data)
      write.csv(updated_data, "data/raw_data.csv", row.names = FALSE)
      
      # Reset form
      updatenumericInput(session, "new_ID", value = max(updated_data$ID, na.rm = TRUE) + 1)
      updateTextInput(session, "new_nume", value = "")
      updateTextInput(session, "new_Prenume", value = "")
      updatenumericInput(session, "new_age", value = 30)
      updateSelectInput(session, "new_gender", selected = "M")
      updateTextInput(session, "new_region", value = "Urban")
      updatenumericInput(session, "new_weight", value = 70)
      updatenumericInput(session, "new_height", value = 170)
      updateTextInput(session, "new_bp", value = "120/80")
      updateSelectInput(session, "new_cholesterol", selected = "Normal")
      updatenumericInput(session, "new_glucose", value = 100)
      updateSelectInput(session, "new_smoker", selected = "No")
      updateSelectInput(session, "new_diabetic", selected = "No")
      updateSelectInput(session, "new_activity", selected = "Moderate")
      
      showNotification("Pacient adăugat cu succes!", type = "message")
    }, error = function(e) {
      showNotification(paste("Eroare la adăugare:", e$message), type = "error")
    })
  })
  
  # Delete patient function
  observeEvent(input$delete_patient_btn, {
    req(input$delete_patient)
    
    tryCatch({
      updated_data <- raw_data() %>% 
        filter(ID != as.numeric(input$delete_patient))
      
      raw_data(updated_data)
      write.csv(updated_data, "data/raw_data.csv", row.names = FALSE)
      
      showNotification("Pacient șters cu succes!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Eroare la ștergere:", e$message), type = "error")
    })
  })
  
  output$total_patients <- renderValueBox({
    data <- processed_data()
    
    # Filter by search if needed
    if (!is.null(input$patient_search) && input$patient_search != "") {
      search_term <- tolower(input$patient_search)
      data <- data %>%
        filter(grepl(search_term, tolower(paste(Nume, Prenume, ID))))
    }
    
    # Total patients output
    valueBox(
      nrow(data),
      "Total Pacienti",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  # Average Varsta output
  output$avg_age <- renderValueBox({
    valueBox(
      round(mean(processed_data()$Varsta, na.rm = TRUE)),
      "Varsta Medie"
    )
  })
  
  # High risk patients output
  output$high_risk <- renderValueBox({
    high_risk_count <- sum(processed_data()$risk_score >= 2, na.rm = TRUE)
    valueBox(
      high_risk_count, 
      "Pacienti Risc Ridicat", 
      icon = icon("exclamation-triangle")
    )
  })
  
  output$risk_plot <- renderPlot({
    ggplot(processed_data(), aes(x = Varsta, y = risk_score, color = Gen)) +
      geom_point(size = 3) +
      geom_smooth(method = "lm") +
      labs(title = "Scor de Risc pe Vârstă", x = "Vârstă", y = "Scor Risc")
  })
  
  output$patient_list <- renderDT({
    datatable(processed_data() %>% 
                select(Nume, Prenume, ID, Varsta, Gen, risk_score),
              options = list(pageLength = 5,
                             columnDefs = list(
                               list(targets = 0, title = "Nume"),
                               list(targets = 1, title = "Prenume")
                             )))
  })
}

# Run the application
shinyApp(ui = ui, server = server)

