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
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Albumin Data Exploration"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          conditionalPanel(condition = "input.tabs == 'Cluster'",
            sliderInput("clusters.Input",
                        "Number of Clusters:",
                        min = 3,
                        max = 12,
                        step = 1,
                        value = 3)
        ),
            sliderInput("albumin.Input",
                        "Albumin:",
                        min = 1,
                        max = 10,
                        step = 0.5,
                        value = c(2, 10)),
            conditionalPanel(condition = "input.tabs == 'VitaminB12' || input.tabs == 'Cluster'",
              sliderInput("vitB12.Input",
                        "Vitamin B12:",
                        min = 0,
                        max = 2000,
                        step = 50,
                        value = c(100, 1000))
            ),
            conditionalPanel(condition = "input.tabs == 'VitaminD' || input.tabs == 'Cluster'",
              sliderInput("vitD.Input",
                        "Vitamin D:",
                        min = 0,
                        max = 100,
                        step = 1,
                        value = c(10, 80))
              ),
        conditionalPanel(
          condition = "input.tabs != 'Cluster'",
            sliderInput("Age.Group",
                        "Age Group:",
                        min = 0,
                        max = 110,
                        step = 10,
                        value = c(10, 40))
        ),
        conditionalPanel(
          condition = "input.tabs != 'Cluster'",
        sliderInput("bmiInput", 
                    label = "BMI Range", 
                    min = 0, 
                    max = 60, 
                    step = 10,
                    value = c(10,30))
        )
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            type = "tabs", id="tabs",
            tabPanel("VitaminD", fluidRow( 
              column(12, plotOutput("vitD") ),
              column(12,"Data Table Vitamint D", DTOutput("datatable"))
            )),
            tabPanel("VitaminB12", fluidRow(  
              column(12, plotOutput("vitB12")),
              column(12, "Data Table B12", DTOutput("b12Table"))
            )),
            tabPanel("Cluster", fluidRow( 
             column(12, plotlyOutput("cluster")),
             column(12, "Cluster Data", DTOutput("clusterTable")),
            )
          )
        )
    )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  set.seed(417)
  
  data <- read_excel("MayaMD_1587.xlsx")
  df <- data.frame(data)
  
  print("entrando")
  df$Age.Group <-  as.integer( ( as.integer( format( Sys.Date(), "%Y") ) - as.integer(format(as.Date(df$Date.of.birth), "%Y")) ) / 10) * 10
  df$Age.Index <- df$Age.Group / 10
  
  df$BMI.Group <-  as.integer(as.integer(df$BMI) / 10) * 10
  df$BMI.Index <- df$BMI.Group / 10 
  
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
  
  CENTERS <- 4
  
  df_albumin <- df_mod %>% filter(!is.na(Albumin...Serum) & !is.na(Vit.D.assay) & !is.na(Vitamin.B12..Serum)) 
  albumin_kmean <- df_mod %>% filter(!is.na(Albumin...Serum) & !is.na(Vit.D.assay) & !is.na(Vitamin.B12..Serum)) %>% select(Albumin...Serum, Vit.D.assay, Vitamin.B12..Serum)
  df_bmi <- df_mod %>% filter(!is.na(Albumin...Serum) & !is.na(Vit.D.assay) & !is.na(Vitamin.B12..Serum))  %>% select(Albumin...Serum, Vit.D.assay, Vitamin.B12..Serum, BMI.Group)
  df_B12 <- df_mod %>% filter(!is.na(Albumin...Serum) & !is.na(Vit.D.assay) & !is.na(Vitamin.B12..Serum))  %>% select(Albumin...Serum, Vit.D.assay, Vitamin.B12..Serum, BMI.Group) 
  df_table <- df_mod %>% filter(!is.na(Albumin...Serum) & !is.na(Vit.D.assay) & !is.na(Vitamin.B12..Serum))  %>% select(Albumin...Serum, Vit.D.assay, Vitamin.B12..Serum, BMI.Group)
  
  header_str <- paste("MK-means ", CENTERS, " cluster", sep=" ")
  
 output$datatable <-  renderDT( dataD() ,  options = list(lenghtChange = FALSE))
   
  #
  # output$vitD <- renderPlot({
  #
  output$vitD <- renderPlot({
          output$datatable <-  renderDT(dataD(), options = list(lenghtChange = FALSE))
    
        fig <- dataD() %>%  
          ggplot(aes(x=Albumin...Serum, y = Vit.D.assay)) + 
          geom_point(aes(col=factor(Age.Group), shape=factor(BMI.Group)), size=2) + scale_shape_discrete(solid=TRUE) + 
          xlim(1,6) + ylim(0,100) +
            geom_hline(yintercept = 3.0, linetype = 'dotted', col = 'red')  + annotate("text", x = 0, y = 3.0, label = "Vit.D 3", vjust=-0.5) +
            geom_vline(xintercept = 3.5, linetype = 'dotted', col = 'blue') + annotate("text", x = 3.5, y = 100.0, label = "Albumin 3.5", hjust=0.5) 
        fig
  })
    
  
  clusterData <- reactive({
      df_albumin %>% filter(Albumin...Serum >= input$albumin.Input[1] &  Albumin...Serum <= input$albumin.Input[2] & 
                                 Vit.D.assay >= input$vitD.Input[1] &  Vit.D.assay <= input$vitD.Input[2] &   
                                 Vitamin.B12..Serum >= input$vitB12.Input[1] & Vitamin.B12..Serum <= input$vitB12.Input[2]) %>%
      select(Gender,  
             Village, 
             Block, 
             District, 
             BMI, 
             Vit.D.assay, 
             Vitamin.B12..Serum, 
             Albumin...Serum, 
             Age.Group, 
             BMI.Group 
      )
  })
  
  #
  # output$cluster <- renderPlotly({
  #
  output$cluster <- renderPlotly({
      numClusters <- as.numeric(input$clusters.Input)
      
      set.seed(240) # Setting seed
      albumin_kmean <- albumin_kmean %>% filter(Albumin...Serum >= input$albumin.Input[1] &  Albumin...Serum <= input$albumin.Input[2] &
                                          Vit.D.assay >= input$vitD.Input[1] &  Vit.D.assay <= input$vitD.Input[2] &  
                                                  Vitamin.B12..Serum >= input$vitB12.Input[1] & Vitamin.B12..Serum <= input$vitB12.Input[2])
      
      kmeans.re <- kmeans(albumin_kmean, centers = numClusters, nstart = 3)
      kmeans.re
      albumin_axis <- albumin_kmean$Albumin...Serum
      vitaminD_axis <- albumin_kmean$Vit.D.assay
      vitaminB12_axis <- albumin_kmean$Vitamin.B12..Serum
      
      output$clusterTable <- renderDT(clusterData(), options = list(lenghtChange = FALSE))
  
      color <- kmeans.re$cluster
  
      cluster_plot <- plot_ly(x=albumin_axis, y=vitaminB12_axis, z=vitaminD_axis, type="scatter3d", mode="markers", color=factor(color), size = 2 )
      cluster_plot <- cluster_plot %>% layout(autosize = T, width=600, height=400, scene = list(xaxis = list(title="Albumin"), yaxis=list(title="Vitamin B12"), zaxis=list(title="Vitamin D")) )
      cluster_plot
  })
    
  dataD <- reactive({ df_albumin %>%  
      filter( Albumin...Serum >= input$albumin.Input[1] & Albumin...Serum <= input$albumin.Input[2] &  
                BMI.Group >= input$bmiInput[1] &  BMI.Group <= input$bmiInput[2] &  
                Vit.D.assay >= input$vitD.Input[1] &  Vit.D.assay <= input$vitD.Input[2] &  
                Age.Group >= input$Age.Group[1] & Age.Group <= input$Age.Group[2]) %>%   
      select(Gender,  
             Village, 
             Block, 
             District, 
             BMI, 
             Vit.D.assay, 
             Vitamin.B12..Serum, 
             Albumin...Serum, 
             Age.Group, 
             BMI.Group 
             )
  })
  
  dataB12 <- reactive({
    df_albumin %>% filter(Albumin...Serum >= input$albumin.Input[1] & Albumin...Serum <= input$albumin.Input[2] & 
                            Vitamin.B12..Serum >= input$vitB12.Input[1] & Vitamin.B12..Serum <= input$vitB12.Input[2] & 
                            BMI.Group >= input$bmiInput[1] & BMI.Group <= input$bmiInput[2] &  
                            Age.Group >= input$Age.Group[1] & Age.Group <= input$Age.Group[2] ) %>% 
      select(Gender,
             Village,   
             Block,   
             District,   
             BMI,   
             Vit.D.assay,   
             Vitamin.B12..Serum,   
             Albumin...Serum,   
             Age.Group,   
             BMI.Group   
             )
  })
  #
  # output$vitB12 <- renderPlot({
  #
  output$vitB12 <- renderPlot({ 
    df_B12 <- df_albumin %>% filter(Albumin...Serum >= input$albumin.Input[1] & Albumin...Serum <= input$albumin.Input[2] &
          Vitamin.B12..Serum >= input$vitB12.Input[1] & Vitamin.B12..Serum <= input$vitB12.Input[2] &
          BMI.Group >= input$bmiInput[1] & BMI.Group <= input$bmiInput[2] & 
          Age.Group >= input$Age.Group[1] & Age.Group <= input$Age.Group[2] 
   )  
      
   # output$b12Table <- renderDT(df_B12, options = list(lenghtChange = FALSE))
   output$b12Table <- renderDT(dataB12(), options = list(lenghtChange = FALSE))
   
  figVitB12 <- dataB12() %>% ggplot(aes(x=Albumin...Serum, y = Vitamin.B12..Serum)) + geom_point(aes(shape=factor(BMI.Group), col=factor(Age.Group) )) + xlim(0,6) + 
          geom_hline(yintercept = 50.0, linetype = 'dotted', col = 'red')  + annotate("text", x = 0, y = 50.0, label = "B12 50", vjust=-0.5) +
          geom_vline(xintercept = 3.5, linetype = 'dotted', col = 'blue') + annotate("text", x = 3.5, y = 1000.0, label = "Albumin 3.5", hjust=0.5) 
  figVitB12
  })
}    

# Run the application 
shinyApp(ui = ui, server = server)
