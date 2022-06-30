#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# install.packages("plotly", repos="http://cran.rstudio.com/", dependencies=TRUE)

library(shiny)
library(readxl)
library(ggplot2)
library(tidyverse)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Albumin Data Exploration"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("albumin.Input",
                        "Albumin:",
                        min = 0,
                        max = 10,
                        value = 5,
                        step = 0.5),
            sliderInput("vitB12.Input",
                        "Vitamin B12:",
                        min = 0,
                        max = 2000,
                        step = 50,
                        value = 1000),
            sliderInput("vitD.Input",
                        "Vitamin D:",
                        min = 0,
                        max = 100,
                        step = .5,
                        value = 40),
        sliderInput("bmiInput", 
                    label = "BMI Range", 
                    min = 0, 
                    max = 60, 
                    step = 10,
                    value = c(10,30))
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("Albumin - Vitamin D", plotOutput("plot1")),
            tabPanel("Albumin -Vitamin B12", plotOutput("plot3")),
            tabPanel("Two", plotlyOutput("plot2")),
            tabPanel("Four", plotOutput("plot4")),
            tabPanel("Five", plotOutput("plot5")),
            tabPanel("Six", plotOutput("plot6")),
            tabPanel("Seven", plotOutput("plot7")),
            tabPanel("Eight", plotOutput("plot8"))
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  set.seed(417)
  temp <- rnorm(100, mean=30, sd=5)
  pressure <- rnorm(100)
  dtime <- 1:100
  
  data <- read_excel("MayaMD_1587.xlsx")
  df <- data.frame(data)
  
  print("entrando")
  df$Age.Group <-  as.integer( ( as.integer( format( Sys.Date(), "%Y") ) - as.integer(format(as.Date(df$Date.of.birth), "%Y")) ) / 10) * 10
  
  df$BMI.Group <-  as.integer(as.integer(df$BMI) / 10) * 10
  
  df_mod <- df
  df_mod$Total.Protein...Serum <- as.numeric(df_mod$Total.Protein...Serum)
  df_mod$Total.Bilirubin...Serum <- as.numeric(df_mod$Total.Bilirubin...Serum)
  
  df_mod$Direct.Bilirubin...Serum <- as.numeric(df_mod$Direct.Bilirubin...Serum);
  df_mod$Indirect.Bilirubin...Serum <- as.numeric(df_mod$Indirect.Bilirubin...Serum);
  df_mod$Albumin...Serum <- as.numeric(df_mod$Albumin...Serum);
  df_mod$Alkaline.Phosphatase...Serum <- as.numeric(df_mod$Alkaline.Phosphatase...Serum);
  df_mod$Alanine.Transaminase..SGPT.ALT....Serum <- as.numeric(df_mod$Alanine.Transaminase..SGPT.ALT....Serum);
  df_mod$Aspartate.Transaminase..SGOT.AST....Serum <- as.numeric(df_mod$Aspartate.Transaminase..SGOT.AST....Serum);
  df_mod$SGOT...SGPT.Ratio <- as.numeric(df_mod$SGOT...SGPT.Ratio);
  df_mod$Globulin...Serum <- as.numeric(df_mod$Globulin...Serum);
  df_mod$Vit.D.assay <- as.numeric(df_mod$Vit.D.assay)
  df_mod$Vitamin.B12..Serum <- as.numeric(df_mod$Vitamin.B12..Serum)
  
  CENTERS = 3
  df_albumin <- df_mod %>% filter(!is.na(Albumin...Serum) & !is.na(Vit.D.assay) & !is.na(Vitamin.B12..Serum)) 
  albumin_kmean <- df_mod %>% filter(!is.na(Albumin...Serum) & !is.na(Vit.D.assay) & !is.na(Vitamin.B12..Serum)) %>% select(Albumin...Serum, Vit.D.assay, Vitamin.B12..Serum)
  
  # Fitting K-Means clustering Model to training dataset
  
  set.seed(240) # Setting seed
  kmeans.re <- kmeans(albumin_kmean, centers = CENTERS, nstart = 10)
  kmeans.re
  
  header_str <- paste("MK-means ", CENTERS, " cluster", sep=" ")
  
  # plot(albumin_kmean[c("Albumin...Serum", "Vit.D.assay")],
  # col = kmeans.re$cluster)
  # plot(albumin_kmean[c("Albumin...Serum", "Vit.D.assay")],
  # col = kmeans.re$cluster,
  # main = header_str )
  
  # kmeans.re$centers
  # kmeans.re$centers[, c("Albumin...Serum", "Vit.D.assay")]
  
    dat <- data.frame(xvar = 1:20 + rnorm(20, sd=3),
                      yvar = 1:20 + rnorm(20, sd=3))
    dat2 <- data.frame(xvar = 1:100 + rnorm(100, sd=3),
                      yvar = 1:200 + rnorm(200, sd=3))
    dat3 <- data.frame(xvar = 1:10 + rnorm(10, sd=3),
                      yvar = 1:10 + rnorm(10, sd=3))
    
    output$plot1 <- renderPlot({
        bmiMin <- as.numeric(input$bmiInput[1])
        bmiMax <- as.numeric(input$bmiInput[2])
        vitD <- as.numeric(input$vitD.Input)
        albuminInput <- as.numeric(input$albumin.Input)
    
        df_bmi <- df_albumin %>% filter(BMI.Group >= bmiMin & BMI.Group <= bmiMax & Vit.D.assay < vitD & Albumin...Serum < albuminInput)  
      
        fig <- df_bmi %>% ggplot(aes(x=Albumin...Serum, y = Vit.D.assay)) + geom_point(aes(col=factor(BMI.Group))) + xlim(0,6) + ylim(0,100) +
          geom_hline(yintercept = 3.0, linetype = 'dotted', col = 'red')  + annotate("text", x = 0, y = 3.0, label = "Vit.D 3", vjust=-0.5) +
          geom_vline(xintercept = 3.5, linetype = 'dotted', col = 'blue') + annotate("text", x = 3.5, y = 100.0, label = "Albumin 3.5", hjust=0.5) 
        fig
    })
    
    output$plot2 <- renderPlotly({
      plot_ly(x=temp, y=pressure, z=dtime, type="scatter3d", mode="markers", color=temp)
    })
    
    output$plot3 <- renderPlot({
        bmiMin <- as.numeric(input$bmiInput[1])
        bmiMax <- as.numeric(input$bmiInput[2])
        vitD <- as.numeric(input$vitD.Input)
        albuminInput <- as.numeric(input$albumin.Input)
        vitB12.Input <- as.numeric(input$vitB12.Input)
    
        df_vitB12 <- df_albumin %>% filter(BMI.Group >= bmiMin & BMI.Group <= bmiMax & Vitamin.B12..Serum < vitB12.Input & Albumin...Serum < albuminInput)  
      
        figVitB12 <- df_vitB12 %>% ggplot(aes(x=Albumin...Serum, y = Vitamin.B12..Serum)) + geom_point(aes(col=factor(BMI.Group))) + xlim(0,6) + 
          geom_hline(yintercept = 50.0, linetype = 'dotted', col = 'red')  + annotate("text", x = 0, y = 50.0, label = "B12 50", vjust=-0.5) +
          geom_vline(xintercept = 3.5, linetype = 'dotted', col = 'blue') + annotate("text", x = 3.5, y = 1000.0, label = "Albumin 3.5", hjust=0.5) 
        figVitB12
    })
    output$plot4 <- renderPlot({
        fig <- dat2 %>% ggplot(aes(x=xvar, y = yvar)) + geom_point()
        fig
    })
    output$plot5 <- renderPlot({
        fig <- dat2 %>% ggplot(aes(x=xvar, y = yvar)) + geom_point()
        fig
    })
    output$plot6 <- renderPlot({
        fig <- dat2 %>% ggplot(aes(x=xvar, y = yvar)) + geom_point()
        fig
    })
    output$plot7 <- renderPlot({
        fig <- dat2 %>% ggplot(aes(x=xvar, y = yvar)) + geom_point()
        fig
    })
    output$plot8 <- renderPlot({
        fig <- dat2 %>% ggplot(aes(x=xvar, y = yvar)) + geom_point()
        fig
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
