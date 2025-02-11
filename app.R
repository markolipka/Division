library(shiny)
library(shinyjs)
library(tidyverse)

richtig_feedback_text <- c("Richtig!",
                           "Genau!",
                           "Klasse!!",
                           "Super!",
                           "Stimmt!",
                           "Perfekt!",
                           "Toll gemacht!",
                           "Fantastisch!",
                           "Prima!",
                           "So ist es!",
                           "Wunderbar!",
                           "Bravo!!",
                           "Supidupi!",
                           "Superkalifragelistik-\nexpialigetisch!!",
                           "Ganz toll!",
                           "Sehr gut!!",
                           "Sehr schön!",
                           "Gut gemacht!",
                           "Hervorragend!",
                           "Spitze!",
                           "Supi!!!")
richtig_feedback_symbol <- c("🎉",
                             "✅", 
                             "✨",
                             "😁",
                             "😃",
                             "😄",
                             "😉",
                             "😍",
                             "😘",
                             "😻",
                             "🚀",
                             "⭐",
                             "🌈",
                             "🌟",
                             "🌸",
                             "🍓",
                             "🍡",
                             "🍩",
                             "🍪",
                             "🍫",
                             "🍬",
                             "🍭",
                             "🍰",
                             "🎁",
                             "🎆",
                             "🎈",
                             "🎊",
                             "🐒",
                             "🐛",
                             "🐠",
                             "🐥")

alleAufgaben <- expand_grid(quotient = 0:10, divisor = 0:10) |>
  mutate(divident = quotient * divisor) |>
  filter(divisor != 0) |> # Dividieren durch 0 ist nicht definiert!
  rowwise() |>
 # mutate(aufgabe = paste(divident, " ÷ ", divisor, " = ?")) |>
  ungroup()

calc_log_stat <- function(){
  
  log <- read.csv(file = "log.csv") |>
    mutate(aufgabe = paste(divident, " ÷ ", divisor, " = ?"),
           zeitstempel = as_datetime(zeitstempel),
           zeitstempel_geklickt = as_datetime(zeitstempel_geklickt),
           dauer = as.numeric(zeitstempel_geklickt - zeitstempel)) |>
    # nur die letzten 5 Antworten jeder Aufgabe berücksichtigen:
    group_by(aufgabe) |>
    mutate(n_Aufgabe = row_number()) |>
    slice_max(n_Aufgabe, n = 5) |> 
    ungroup()
  log_stat <- log |>
    left_join(x = alleAufgaben, y = _, 
              by = c("divident", "divisor", "quotient")) |>
    summarise(trefferquote = mean(richtig),
              mittl_dauer = median(dauer),
              .by = c(divident, divisor, quotient))
}

# UI
ui <- fluidPage(
  useShinyjs(),
  # titlePanel("Kleines 1x1 üben"),
  sidebarLayout(
    sidebarPanel(width = 5,
                 h1("Hi Neli!"),
                 h1(textOutput("task")),
                 br(),
                 h3(textOutput("feedback")),
                 br(),
                 plotOutput("statPlot", width = 400, height = 400)
    ),
    mainPanel(width = 7,
              h4("Wähle die richtige Antwort:"),

              fluidRow(
                lapply(0:10, function(j) {
                  actionButton(
                    inputId = paste0("btn_", j),
                    label = j,
                    style = "width: 50px; height: 50px; margin: 2px; background-color: lightgray;"
                  )
                })
              )
              
    )
  )
)

# Server
server <- function(input, output, session) {

  output$statPlot <- renderPlot({
    task()
    update_plot()
    
    log_stat <- calc_log_stat()
    
    if(all(is.na(log_stat$trefferquote))){return(NA)}
    
    if(all(log_stat$trefferquote == 1, na.rm = TRUE) & all(!is.na(log_stat$trefferquote))){
      random_image_file <- list.files(path = "images",
                                      pattern = "\\.jpeg$") |>
        sample(size = 1)
      library(jpeg)
      img <- jpeg::readJPEG(source = file.path("images", random_image_file))
      # plot with picture as layer
      ggplot() +
        annotation_raster(img, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
    }else{
      ggplot(log_stat) +
        aes(x = quotient, y = divisor, fill = trefferquote, color = mittl_dauer) +
        geom_tile(alpha = 1) +
        scale_fill_gradient2(low = "coral",
                             mid =  "gold", 
                             high = "forestgreen",
                             midpoint = .5,
                             limits = c(0, 1),
                             na.value = "white") +
        geom_point(aes(alpha = mittl_dauer), size = 4) +
        scale_color_gradient(low = "forestgreen", 
                             high =  "pink", 
                             limits = c(0, 30),
                             na.value = "white") +
        theme_void() +
        theme(legend.position = "none")
    }
  })
  
  update_plot <- reactiveVal(Sys.time())
  
  task <- reactiveVal(list(timestamp = lubridate::now(),
                           divident = 0, 
                           divisor = 0, 
                           quotient = 0))
  
  generateTask <- function() {
    log_stat <- calc_log_stat()
                  
    # Zufallsziehung einer Aufgabe mit Gewichten:
    aufgabe <- log_stat |>
      mutate(trefferquote = replace_na(trefferquote, 0),
             mittl_dauer = replace_na(mittl_dauer, 99)) |>
      ## erstmal n vällig zufällige Aufgaben ziehen:
      slice_sample(prop = .42) |>
      ## daraus dann die Hälfte gewichtet nach Dauer (also langsame bevorzugen):
      slice_sample(prop = .5, weight_by = mittl_dauer) |> 
      ## schließlich Ziehung der neuen Aufgabe mit geringster Trefferquote
      slice_min(trefferquote, n = 1, with_ties = FALSE) |> 
      mutate(quotient = divident / divisor)
    
    task(list(timestamp = lubridate::now(),
              divident = aufgabe$divident,
              divisor = aufgabe$divisor,
              quotient = aufgabe$quotient))
  }
  
  generateTask()
  
  output$task <- renderText({
    paste(task()$divident, "÷", task()$divisor, "= ?")
  })
  
  observe({
    lapply(0:10, function(num) {
      observeEvent(input[[paste0("btn_", num)]], {
        ### log Eingabe:
        data.frame(ts_gen = task()$timestamp,
                   divident = task()$divident,
                   divisor = task()$divisor,
                   quotient = task()$quotient,
                   clicked = num,
                   ts_clicked = lubridate::now(),
                   task()$quotient == num) |>
          write.table(file = "log.csv", sep = ",", 
                      append = TRUE, 
                    row.names = FALSE, col.names = FALSE)
        
        btn_id <- paste0("btn_", num)
        if (num == task()$quotient) {
          output$feedback <- renderText(paste(sample(richtig_feedback_symbol, size = 1),
                                              sample(richtig_feedback_text, size = 1),
                                              sample(richtig_feedback_symbol, size = 1),
                                              collapse = " "))
          runjs(sprintf("$('#%s').css('background-color', 'lightgreen');", btn_id))
          Sys.sleep(1)
          generateTask()
          lapply(0:100, function(n) {
            runjs(sprintf("$('#btn_%s').css('background-color', 'lightgray');", n))
          })
        } else {
          output$feedback <- renderText("Das war nicht richtig. Versuch es nochmal!")
          runjs(sprintf("$('#%s').css('background-color', 'lightcoral');", btn_id))
          Sys.sleep(1)
          runjs(sprintf("$('#%s').css('background-color', 'lightgray');", btn_id))
          update_plot(Sys.time())
        }
      })
    })
  })
}

# App starten
shinyApp(ui, server)
