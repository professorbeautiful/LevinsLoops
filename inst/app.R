library(LevinsLoops)

data("cm.levins", package="LoopAnalyst")
nodeNames = rownames(cm.levins)

makeSliders = function()  {
  M = cm.levins
  nameGrid = expand.grid(rownames(M), rownames(M), stringsAsFactors = FALSE)
  #print(nameGrid)
  returnVal = lapply(1:nrow(nameGrid),
                     function(linkNum) {
                       nodes = unlist(nameGrid[linkNum, ])
                       node_to = nodes[1]
                       node_from = nodes[2]
                       numericInput(inputId = nodeNameID(node_from, node_to),
                                    label = nodeNameLabel(node_from, node_to),
                                    min = -1.5, max = 1.5,
                                    value = M[node_to, node_from],
                                    step = 0.01)
                     }
  )
  returnVal = lapply(
    split(1:length(returnVal),
          1 + (-1 + 1:length(returnVal)) %% length(nodeNames)
    ),
    function(sliders) fluidRow(
      lapply(returnVal[sliders], column, width=1)
    ))  #shiny::tagAppendAttributes()
  returnVal
}


nodeNameID = function(n1, n2) paste0("Input", n1, n2, sep="_")
nodeNameLabel = function(n1, n2) paste(n1, n2, sep="->")

server = function(input, output, session) {
  rValues = reactiveValues()
  make.M = reactive({
    #   if(!is.null(input$X1X1))
    #     M = cbind(c(input$X1X1, input$X1X2),
    #               c(input$X2X1, input$X2X2) )
    #   else
    M = cm.levins
    Mtry = try({
      for(X1 in nodeNames)
        for(X2 in nodeNames)
          M[X2,X1] = input[[nodeNameID(X1,X2)]]
    })
    print(M)
    print(Mtry)
    print(class(Mtry))
    if(class(Mtry) == 'try-error'
       | is.null(Mtry)) returnVal = M
    else returnVal = Mtry
    (returnVal)
  })

  output$table = renderTable({
    make.M()
  })
  output$effectMatrix = renderTable({
    cat("effectMatrix\n")
    print(attr(rValues$result, "effectMatrix"))
  })
output$predictedEq = renderTable({
  cat("predictedEq:\n")
  predictedEq = attr(rValues$result, "predictedEq")
  predictedEq =  as.data.frame(as.list(predictedEq))
  rownames(predictedEq) = "predicted equilibrium"
  print(predictedEq)
})
  output$plot = renderPlot({
    library(LevinsLoops)
    result = rValues$result =
      dynamSim(M = make.M(), attachAttributes=TRUE, returnLast=TRUE)
    abline(h=result)
    if(exists("previousResult"))
      abline(h=previousResult, lty=2)
    previousResult <<- result
  })
}

ui = fluidPage(
  makeSliders(),
  tableOutput("predictedEq"),
  fluidRow(  column(2, "Community matrix:"), column(4, tableOutput("table")),
             #"SPACEHOLDER"),
                    column(2, "Effects:"), column(4, tableOutput("effectMatrix"))),
  plotOutput("plot")
)

shinyApp(ui = ui, server = server)

