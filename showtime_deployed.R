# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

# Front-End ----
ui <- fluidPage(
  theme = shinythemes::shinytheme("cyborg"),
  navbarPage(
    "Plotting some built-in datasets",
    ## Iris ----
    tabPanel(
      "Iris",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "x_var_iris",
            "x axis",
            choices = names(iris)
          ),
          selectInput(
            "y_var_iris",
            "y axis",
            choices = names(iris)
          )
        ),
        mainPanel(
          p(
            "This famous (Fisher's or Anderson's)
            iris data set gives the measurements
            in centimeters of the variables sepal
            length and width and petal length and
            width, respectively, for 50 flowers
            from each of 3 species of iris.
            The species are Iris setosa, versicolor,
            and virginica.",
            style = "color:white"
          ),
          print(
            ""
          ),
          plotOutput(
            "grafico_iris"
          )
        )
      )
    ),
    ## USArrests ----
    tabPanel(
      "USArrests",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "x_var_USA",
            "x axis",
            choices = names(USArrests)
          ),
          selectInput(
            "y_var_USA",
            "y axis",
            choices = names(USArrests)
          )
        ),
        mainPanel(
          p(
            "This data set contains statistics,
            in arrests per 100,000 residents for
            assault, murder, and rape in each of
            the 50 US states in 1973. Also given
            is the percent of the population
            living in urban areas.",
            style = "color:white"
          ),
          plotOutput("grafico_USA")
        )
      )
    )
    ## Your dataset!! ----
    ,tabPanel(
      "Your dataset!!",
      sidebarLayout(
        sidebarPanel(
          fileInput(
            "file_input",
            label = "Gimme a csv file",
            accept = ".csv"
          ),
          selectInput(
            "x_var_db",
            "x axis",
            choices = NULL
          ),
          selectInput(
            "y_var_db",
            "y axis",
            choices = NULL
          )
        ),
        mainPanel(
          p(
            "You probably know more
            about this dataset than me.",
            style = "color:white"
          ),
          print(
            ""
          ),
          plotOutput(
            "grafico_db"
          )
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
# Back-End ----
server <- function(input, output, session) {
  ## Iris ----
  output$grafico_iris <- renderPlot(
    plot(
      x = iris[,colnames(iris) == input$x_var_iris],
      y = iris[,colnames(iris) == input$y_var_iris],
      xlab = input$x_var_iris,
      ylab = input$y_var_iris
    )
  )
  
  ## USArrests ----
  output$grafico_USA <- renderPlot(
    plot(
      x = USArrests[,colnames(USArrests) == input$x_var_USA],
      y = USArrests[,colnames(USArrests) == input$y_var_USA],
      xlab = input$x_var_USA,
      ylab = input$y_var_USA
    )
  )
  
  ## Your dataset!! ----
  base <- reactive({
    base <- input$file_input
    if(is.null(base)) return (NULL)
    base <- read.csv(
      base$datapath,
      sep = ",",
      encoding = "UTF-8",
    )
    return(base)
  })

  observe({
    updateSelectInput(
      session,
      inputId = 'x_var_db',
      label = 'x_var',
      choices = names(base()),
      selected = names(base())[1]
    )
    updateSelectInput(
      session,
      inputId = 'y_var_db',
      label = 'y_var',
      choices = names(base()),
      selected = names(base())[2]
    )
  })

  output$grafico_db <- renderPlot({
    db <- base()
    if(is.null(base())){
      return(plot.new())
    }
    graphic <- plot(
      x = db[,names(db) == input$x_var_db],
      y = db[,names(db) == input$y_var_db],
      xlab = input$x_var_db,
      ylab = input$y_var_db
    )
    graphic
  })
}

# Run the application ----
shinyApp(ui = ui, server = server)                                                                                                                                                                                                                                                                                                                                          