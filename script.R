if (!require("shiny")) install.packages("shiny")
if (!require("corrplot")) install.packages("corrplot")
if (!require("GGally")) install.packages("GGally")
if (!require("DT")) install.packages("DT")
if (!require("manhattanly")) install.packages("manhattanly")
if (!require("h2o")) install.packages("h2o")
if (!require("cluster")) install.packages("cluster")
if (!require("dplyr")) install.packages("dplyr")
if (!require("philentropy")) install.packages("philentropy")


library("qqman")
library("philentropy")
library("h2o")
library("plotly")
library("manhattanly")
library("ggplot2")
library("corrplot")
library("cluster")
library("dplyr")

# ===============================================================================================================================================
#                                                            VARIABLES
# ===============================================================================================================================================

choices <- list("Euclideana", "Manhattan", "Pearson")

# ================================================================================================================================================
#                                                               UI
# ================================================================================================================================================

ui <- fluidPage(
  # ============
  #   NAVABAR
  # ============
  navbarPage("MINERIA DE DATOS"),

  # ===============
  # SIDEBAR LAYOUT
  # ===============
  sidebarLayout(
    # ================
    #   SIDEBAR PANEL
    # ================
    sidebarPanel(
      
      # ================
      #  ARCHIVO
      # ================
      h3("Archivo"),
      fileInput(inputId = "file", label = "Archivo", accept = c(
        "text/csv",
        "text/comma-separated-values,text/plain",
        ".csv"
      )),
      checkboxInput(inputId = "file_is_distance_matrix", value = FALSE, label = "CSV es una matriz de distancia"),
     
     
      hr(),
      
      # ================
      #  CALCULO D/S
      # ================
      h3("Calculo Distancia/Similitud"),
      selectInput(
        inputId = "choice", label = strong("Medida distancia/similitud "),
        choices = choices,
        selected = "Euclideana"
      ),
      selectizeInput(
        "file_columns",
        "Columnas",
        choices = c(),
        multiple = TRUE,
      ),
      numericInput("file_rows", value = 0, label = "Numero de filas"),
     
      # ================
      # BUTTONS
      # ================
      actionButton("submit", "Calcular"),
      downloadLink("downloadData", "Descargar")
     
    ),
    # ============
    #  MAIN PANEL
    # ============
    mainPanel(
      tabsetPanel(
        type = "tabs",
        # ============
        #  DATOS
        # ============
        tabPanel(
          "Datos",
          dataTableOutput("table")
        ),
        # ============
        #  GRAPHIC
        # ============
        tabPanel(
          "Grafico", 
          plotOutput("plot")
        )
      )
    )
  ),
)

# ================================================================================================================================================
#                                                              FUNCTIONS
# ================================================================================================================================================


euclidean_dist <- function(k,unk) {
  # Make distance a vector [although not technically required]
  distance <- rep(0, nrow(k))
  
  for(i in 1:nrow(k))
    # Change unk[,1][i] to unk[1,1] and similarly for unk[,2][i]
    distance[i] <- sqrt((k[,1][i] - unk[1,1])^2 + (k[,2][i] - unk[1,2])^2)
  
  return(distance)
} 

# ================================================================================================================================================
#                                                               SERVER
# ================================================================================================================================================

server <- function(input, output, session) {


  # ================
  # REACTIVE VALUES
  # ================
  dataset <- reactive({
    file <- input$file
    n_rows <- input$file_rows
    columns <- input$file_columns
    
    if (!is.null(file)) {
      return(read.csv(file$datapath, nrows = n_rows, col.names = columns))
    } else {
      return(mtcars)
    }
  })
  
  distance_matrix <- reactive({
    choice <- input$choice
    
    if(choice == "Euclideana"){
      return(dist(dataset(), method = "euclidean"))
    }else if(choice == "Manhattan"){
      return(dist(dataset(), method = "manhattan"))
    }else {
      return(distance(select(dataset(),-1),method = "pearson"))
    }
  })
  # ================
  # OUTPUT TABLE
  # ================
  output$table <- renderDataTable({
    file <- input$file
    
    if (is.null(file)) {
      return(NULL)
    }
    
    read.csv(file$datapath)
  })
  
  # ======================
  # OBSERVER INPUT CHANGE
  # ======================
  observeEvent(input$file,{
    file <- input$file
    
    if (is.null(file)) {
      return(NULL)
    }
    
    # ======================
    # GETTERS
    # ======================
    dataset <-  read.csv(file$datapath)
    n_rows <- nrow(dataset)
    columns <- colnames(dataset, do.NULL = TRUE, prefix = "col")
    
    # ======================
    # SETTERS
    # ======================
    updateNumericInput(session,"file_rows", value = n_rows)
    updateSelectizeInput(session, "file_columns", choices = columns, selected = columns)
  
  })
  
  # ================
  # SUBMIT BUTTON
  # ================
  observeEvent(input$submit,{

    
    is_distance_matrix <- input$file_is_distance_matrix
    choice <- input$choice

    # ==============
    #  OUTPUT PLOT
    # ==============
    if(choice == "Euclideana") {
      print("GRAFICAR EUCLIDEAN")
      output$plot <- renderPlot({
        if(is_distance_matrix == TRUE){
          print("ES DISTANCIA MATRIX")
          hc = hclust( as.dist(as(distance_matrix(), "matrix")))
          plot(hc)
        }else {
          print("NO ES DISTANCIA MATRIX")
          hc <- hclust(distance_matrix(), method="ward.D")
          plot(hc)
        }
      })
    } else if(choice == "Manhattan") {
      print("GRAFICAR MANHATTAN")
      output$plot <- renderPlot({
        if(is_distance_matrix == TRUE){
          hc = hclust( as.dist(as(distance_matrix(), "matrix")))
          plot(hc)
        }else {
          hc <- hclust(distance_matrix(), method="ward.D")
          plot(hc)
        }
      })
    } else {
      print("GRAFICAR PEARSON")
      output$plot <- renderPlot({
        if(is_distance_matrix == TRUE){
          correlation <- cor(dataset())
          corrplot(correlation, method = "circle")
        }else {
          correlation <- cor(select(dataset(), -1))
          corrplot(correlation, method = "circle")
        }
      })
    }
  })
  
  # ================
  # DOWNLOAD BUTTON
  # ================
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    
    content = function(file) {
      print(distance)
      df <-as.data.frame(as.matrix(distance_matrix()))
      write.csv(df, file)
    }
  )

}

# ================================================================================================================================================
#                                                               RUN
# ================================================================================================================================================

shinyApp(ui = ui, server = server)


