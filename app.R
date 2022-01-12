# load package
library(shiny)
library(shinythemes)
library(tidyverse)
library(survival)
library(survminer)
library(DT)

# define ui
ui <- fluidPage(
    navbarPage("Survival Analytic System", theme = shinytheme("united"),
               tabPanel("About",fluid = TRUE,
                        paste("This page is to tell about the data")
                        ),
               tabPanel("Data File", fluid = TRUE,
                        titlePanel("Data Pneumonia"),
                        sidebarLayout(
                            sidebarPanel(
                                fileInput("file", "Silahkan pilih file csv",
                                          accept = c(
                                            "text/csv",
                                            "text/comma-separated-values,text/plain",
                                            ".csv")
                                          ),
                                selectInput("time_variable","Pilih variabel survival time", "NA"),
                                selectInput("event_variable","Pilih variabel survival event", "NA")
                            ),
                            mainPanel(
                              h4("Data Survival"),
                              DTOutput("contents")
                            )
                        )
                        ),
               tabPanel("Cox Model", fluid = TRUE,
                        titlePanel("Cox Model"),
                        sidebarLayout(
                          sidebarPanel(
                            checkboxGroupInput("input_cox","Pilih variabel cox model","NA")
                          ),
                          mainPanel(
                            h4("Cox Model Summary"),
                            verbatimTextOutput("cox")
                          )
                        )
                      ),
               tabPanel("Parametric Model", fluid = TRUE,
                        titlePanel("Parametric Model"),
                        sidebarLayout(
                          sidebarPanel(
                            checkboxGroupInput("input_parametric","Pilih variabel parametric model","NA"),
                            radioButtons("pilih_distribusi","Pilih distribusi yang diasumsikan",
                                         list("Distribusi Weibull" = "weibull",
                                              "Distribusi Exponential" = "exponential",
                                              "Distribusi Log-normal" = "lognormal",
                                              "Distribusi Gaussian" = "gaussian",
                                              "Distribusi Logistic" = "logistic",
                                              "Distribusi Log-logistic" = "loglogistic")
                                         )
                          ),
                          mainPanel(
                            h4("Parametric Model (AFT, Accelerated Failure Time) Summary"),
                            verbatimTextOutput("parametric"),
                            plotOutput("plot")
                          )
                        )
                        ),
               tabPanel("Result of comparing method", fluid = TRUE,
                        titlePanel("Perbandingan Metode Parametrik dan Semi-Parametrik"),
                        sidebarLayout(
                          sidebarPanel(
                            checkboxGroupInput("compare_method","Pilih variabel yang akan dibandingkan","NA")
                          ),
                          mainPanel(
                            h4("Nilai AIC Parametric Model"),
                            verbatimTextOutput("aic_param"),
                            #htmlOutput("aic_param_detail"),
                            h4("Nilai AIC Semi-Parametric Model"),
                            verbatimTextOutput("aic_semi_param"),
                            h4("Kesimpulan:"),
                            htmlOutput("aic_summary")
                          )
                        )
                        )
               )
)

# define server
server <- function(input, output, session){
  data_survival <- reactive({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    a <- read.csv(inFile$datapath, header = TRUE)
  })
  
  observe({
    updateSelectInput(session, "time_variable", "Pilih variabel survival time", names(data_survival()))
    updateSelectInput(session, "event_variable", "Pilih variabel survival event", names(data_survival()))
    updateCheckboxGroupInput(session, "input_cox", "Pilih variabel cox model ",colnames(data_survival()))
    updateCheckboxGroupInput(session, "input_parametric", "Pilih variabel cox model ",colnames(data_survival()))
    updateCheckboxGroupInput(session, "compare_method", "Pilih variabel yang akan dibandingkan",colnames(data_survival()))
  })
  
  output$contents <- DT::renderDT({
    data_survival()
  })
  
  output$cox <- renderPrint({
    if (is.null(input$input_cox)) {
      return(NULL)
    } else {
      inp_cox = paste(input$input_cox, collapse = " + ")
      cov = paste('Surv(',input$time_variable,',',input$event_variable,') ~',
                  inp_cox)
      cox_model = coxph(formula = as.formula(cov),
                        data = data_survival())
      return(summary(cox_model))
    }
  })
  
  output$parametric <- renderPrint({
    if (is.null(input$input_parametric)) {
      return(NULL)
    } else {
      inp_param = paste(input$input_parametric, collapse = " + ")
      aftcov = paste('Surv(',input$time_variable,',',input$event_variable,') ~',
                     inp_param)
      model_param = survreg(formula = as.formula(aftcov),
                            data = data_survival(),
                            dist = input$pilih_distribusi)
      return(summary(model_param))
    }
  })
  
  output$plot <- renderPlot({
    if (is.null(input$input_parametric)) {
      return("")
    } else {
      inp_param = paste(input$input_parametric, collapse = " + ")
      aftcov = paste('Surv(',input$time_variable,',',input$event_variable,') ~',
                     inp_param)
      model_param2 = survfit(formula = as.formula(aftcov),
                             data = data_survival())
      return(plot(model_param2))
    }  
  })
  
  aic_parametrik <- reactive({
    if (is.null(input$compare_method)) {
      return(NULL)
    } else {
      inp_compare = paste(input$compare_method, collapse = " + ")
      dist_names <- c("weibull", "exponential", "lognormal", "gaussian", "logistic", "loglogistic")
      aic <- list()
      for (name in dist_names) {
        aftcov = paste('Surv(',input$time_variable,',',input$event_variable,') ~',
                       inp_compare)
        aic[name] <- list(AIC(
          survreg(formula = as.formula(aftcov),
                  data = data_survival(),
                  dist = name)
        ))
      }
      return(aic)
    }
    
  })
  
  output$aic_param <- renderPrint({
    if (is.null(input$compare_method)) {
      return(NULL)
    } else {
      min = min(unlist(aic_parametrik()))
      return(min)
    }
  })
  
 # output$aic_param_detail <- renderText({
    # a = match(min(unlist(aic_parametrik())), aic_parametrik())
    # paste('Nilai AIC terkecil pada Paramterik Model menggunakan distribusi:',
    #      "<b>",names(aic_parametrik())[a],"</b>")
 # })
  
  aic_semi_parametrik <- reactive({
    if (is.null(input$compare_method)) {
      return(NULL)
    } else {
      inp_compare = paste(input$compare_method, collapse = " + ")
      cov = paste('Surv(',input$time_variable,',',input$event_variable,') ~',
                  inp_compare)
      cox_model = coxph(formula = as.formula(cov),
                        data=data_survival())
      return(AIC(cox_model))
    }
  })
  
  output$aic_semi_param <- renderPrint({
    if (is.null(input$compare_method)) {
      return(NULL)
    } else {
      return(aic_semi_parametrik())
    }
  })
  
  best_model <- reactive({
    if (min(unlist(aic_parametrik())) < aic_semi_parametrik()) {
      return("Model Parametrik")
    } else {
      return("Model Semi-Parametrik")
    }
  })
  
  output$aic_summary <- renderText({
    if (is.null(input$compare_method)) {
      return("")
    } else {
      paste("Model terbaik berdasarkan nilai AIC terkecil adalah",
            "<b>",best_model(),"</b>")
    }
  })
}

# run the application
shinyApp(ui = ui, server = server)
