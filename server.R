library(shiny)
library(ggplot2)

server <- function(input, output) {
  
  values <- reactiveValues()
  
  observe({
    values$true_positive <- round( (input$infections/input$population) * input$sensitivity * input$population) # true positive in percent of population
    values$false_positive <- round( (1-input$infections/input$population) * (1-input$specifity) * input$population) # false positive in percent of population
    values$total_positive <-  values$true_positive + values$false_positive
    values$true_negative <- round( (1-input$infections/input$population) * input$specifity * input$population)
    values$false_negative <- round( (input$infections/input$population) * (1-input$sensitivity) * input$population)
    values$total_negative <- values$true_negative + values$false_negative
    values$percent_positive <- round(values$true_positive/values$total_positive * 100, 2)
    values$percent_negative <- round(values$true_negative/values$total_negative * 100, 2)
    values$table_plot <- data.frame("Part" = c("True positive", "True negative",
                                               "False positive", "False negative"),
                                    "Amount" = c(values$true_positive, values$true_negative,
                                                 values$false_positive, values$false_negative))
  })
  
  output$newvals <- renderUI({
    str1 <- sprintf ("True positives: %s", values$true_positive)
    str2 <- sprintf ("False positives: %s", values$false_positive)
    str3 <- sprintf ("True negatives: %s", values$true_negative)
    str4 <- sprintf ("False negatives: %s", values$false_negative)
    str5 <- sprintf ("Total positives: %s", values$total_positive)
    str6 <- sprintf ("Total negatives: %s", values$total_negative)
    str7 <- sprintf ("Correctly identified positives out of positives: %s percent", values$percent_positive)
    str8 <- sprintf ("Correctly identified negatives out of negatives: %s percent", values$percent_negative)
    HTML(paste(str1, str2, str3, str4, str5, str6, str7, str8, sep = '<br/>'))
  })
  
  output$plots <- renderPlot({
    ggplot(values$table_plot, aes(x=Part, y=Amount, fill=Part))+
      geom_bar(stat="identity", position="stack")+
      theme_minimal()+
      coord_cartesian(expand=F)
  })
  
}