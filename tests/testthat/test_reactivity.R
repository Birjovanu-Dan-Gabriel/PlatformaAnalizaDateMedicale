library(shinytest2)
library(testthat)

test_that("Filtrarea pacienților funcționează corect", {
  # Pregătim date de test
  test_data <- data.frame(
    patient_id = 1:3,
    nume = c("Popescu", "Ionescu", "Ionescu"),
    prenume = c("Ana", "Maria", "Ion"),
    age = c(30, 45, 60),
    stringsAsFactors = FALSE
  )
  
  # Salvăm datele temporar
  temp_csv <- tempfile(fileext = ".csv")
  write.csv(test_data, temp_csv, row.names = FALSE)
  
  # Configurăm aplicația pentru test
  app <- AppDriver$new(
    options = list(
      shiny.testmode = TRUE,
      test.data.path = temp_csv
    ),
    load_timeout = 20000
  )
  
  # Etapa 1: Autentificare
  tryCatch({
    app$set_inputs(`auth-user` = "doctor")
    app$set_inputs(`auth-password` = "parola2")
    app$click("auth-go")
    app$wait_for_idle()
  }, error = function(e) {
    message("Elemente de autentificare negăsite, continuăm fără login...")
  })
  
  # Etapa 2: Testare căutare
  app$set_inputs(`patient_search_term` = "Ionescu")
  app$run_js("$('#patient_search_btn').click();") # Alternativă la click() direct
  
  # Așteptăm actualizare
  Sys.sleep(2) # Pauză pentru actualizare UI
  
  # Verificări
  output <- app$get_value(output = "patient_list")
  
  # Verificăm manual dacă output$data există
  if (!is.null(output$data)) {
    expect_equal(nrow(output$data), 2) # 2 pacienți Ionescu
    expect_true(all(grepl("Ionescu", output$data$nume)))
  } else {
    # Debug: afișăm structura completă a output-ului
    message("Structura output pacient_list:")
    print(names(output))
    expect(FALSE, "Output-ul patient_list nu conține date")
  }
  
  # Curățare
  unlink(temp_csv)
})
