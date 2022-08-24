# server.r ###############################################################################

library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(ggplot2)



shinyServer(function(input, output, session)
{
  theData <- reactive({
    req(input$csv_file)
    read.csv(input$csv_file$datapath, header = T)
  })
  
  output$csv_file_res <- renderPrint({
    head(theData() )
  })
  
  # var sel ############################################################################
  
  observe(
    updateCheckboxGroupInput(session = session, inputId = "var_sel", label = "Select the variables you want to work with, y should be the first variable", choices = names(theData()), selected = names(theData()) )
  )
  
  theData_1 <- reactive({
    req(input$csv_file)
    select( .data = theData(), input$var_sel )
  })
  
  # output$faje <- renderPrint( theData_1() )
  
  # data type #####################################################################
  
  
  observe(
    updateCheckboxGroupInput(session = session, 
                             inputId = "num", 
                             label = "num", 
                             choices = names( theData_1() ), 
                             selected = names( theData_1() )[1]   )
  )
  
  theData_2 <- reactive({
    mutate( theData_1(), across(all_of(input$num), as.numeric ) )
  })
  
  
  observeEvent(input$num,{
    updateCheckboxGroupInput(
      session = session,
      inputId = "cat", 
      label = "cat", 
      choices = names( select( theData_2(), - (input$num) ) ), 
      selected = names( select( theData_2(), - (input$num) ) )[2]
    )
  })
  theData_3 <- reactive({
    mutate( theData_2(), across(all_of(input$cat), as.factor) )
  })
  
  output$theData_3_res <- renderPrint( str(theData_3() ) )
  
  observeEvent(input$var_sel,{
    updateSelectInput(session = session, inputId = "y", label = "y", choices = names(theData_1()) )
  })
  
  # partitioning ########################################################
  
  prob <- reactive({
    input$prob
  })
  
  ind <- reactive({ 
    sample(2, nrow( theData_3() ), replace = T, prob = c( prob()/100 ,1- (prob()/100) ))
  })
  training_data <- reactive({
    theData_3()[ind()==1,]
  }) 
  test_data<-reactive({
    theData_3()[ind()==2,]
  })
  
  f1 <-reactive({ data.frame( 
    data = c("training", "test"),
    no = c(  nrow(training_data()), nrow(test_data() ) )
  )
  })
  
  output$prob_res <- renderPrint( f1() )
  
  # model ############################################################# 
  
  formulation <- reactive({
    as.formula(paste(input$y, "~."))
  })
  
  glm_training <- reactive({
    glm( formulation() , data = training_data(), family = 'binomial')  
  })
  
  output$glm_training_res <- renderPrint({
    glm_training() 
  })
  
  glm_test <- reactive({
    glm( formulation() , data = test_data(), family = 'binomial'  )
  })
  
  output$glm_test_res <- renderPrint({
    glm_test()
  })
  
  # model summary ###########################################################
  
  output$glm_training_summary_res <- renderPrint({
    summary( glm_training()  )
  })
  
  output$glm_test_summary_res <- renderPrint({
    summary( glm_test()  )
  })
  
  
  # residuals ##########################################################
  
  #cut off
  cutoff_val <- reactive({
    input$cut_off
  })
  
  #reactive training
  prediction_1_training <- reactive({
    predict(glm_training() ,type = "response")
  })
  prediction_2_training <- reactive({
    ifelse( predict(glm_training() ,type = "response") > cutoff_val(), 1, 0 )
  })
  actual_training <- reactive({
    training_data()[input$y]
  })
  
  #training
  residual_table_training <- reactive({ 
    data.frame(
      prediction_1 = prediction_1_training(),
      prediction_2 = prediction_2_training(),
      actual     = actual_training()
    )
  })
  
  output$residuals_for_training <- DT::renderDataTable({ residual_table_training() })
  
  #reactive test
  
  prediction_1_test <- reactive({
    predict(glm_test() ,type = "response")
  }) 
  prediction_2_test <- reactive({
    ifelse( predict(glm_test() ,type = "response") > cutoff_val(), 1, 0 )
  }) 
  actual_test  <- reactive({
    test_data()[input$y]
  })
  
  # test  
  residual_table_test <- reactive({ 
    data.frame(
      prediction_1 = prediction_1_test(),
      prediction_2 = prediction_2_test(),
      actual       = actual_test()
    )
  })
  
  output$residuals_for_test <- DT::renderDataTable({ residual_table_test() })
  
  # confusion matrix ########################################################
  
  output$confusoin_matrix_for_traing_set <- renderPrint({
    table(residual_table_training()[, 2] , residual_table_training()[, 3] )
  })    
  
  
  output$confusoin_matrix_for_test_set <- renderPrint({
    table(residual_table_test()[, 2] , residual_table_test()[, 3] )
  })  
  
  # end ###########################################################
  
})


