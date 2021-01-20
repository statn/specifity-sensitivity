ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(sliderInput(inputId = "sensitivity", "Sensitivity", min = .7, max = 1, value = .99, step = .001),
                 sliderInput(inputId = "specifity", "Specifity", min = .7, max = 1, value = .85, step = .001),
                 numericInput(inputId = "population", "Population", value = 100000),
                 numericInput(inputId = "infections", "Infections", value = 150),
                 uiOutput(outputId = "newvals")),
    mainPanel(plotOutput(outputId = "plot"))
  ))