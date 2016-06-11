library(LevinsLoops)
library("LoopAnalyst")
data("cm.levins", package="LoopAnalyst")

makeSliders = function(M = cm.levins)  {
  nodeNames = rownames(M)
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
      lapply(returnVal[sliders], column, width=3)
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
    rValues$M = make.M()
  })
  output$effectMatrix = renderTable({
    cat("effectMatrix\n")
    print(attr(rValues$result, "effectMatrix"))
  })
  output$predictedEq = renderTable({
    cat("predictedEq:\n")
    predictedEq = attr(rValues$result, "predictedEq")
    predictedEq =  as.data.frame(as.list(predictedEq))
    predictedEq = rbind(predictedEq, rValues$result)
    rownames(predictedEq) = c("predicted equilibrium", 'final in simulation')
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
  output$cmPlot = renderImage({
    graph.cm(rValues$M, file="M.graphcm.dot")
    system("dot -Tgif -O M.graphcm.dot",
           ignore.stdout=TRUE, ignore.stderr = TRUE)
    outfile = "M.graphcm.dot.gif"
    list(src = outfile,
         height=300, width=400,
         alt = "CM should be here")
  }, deleteFile = FALSE)

  output$cemPlot = renderImage({
    graph.cem(make.cem(rValues$M), file="M.graphcem.dot")
    system("dot -Tgif -O M.graphcem.dot",
           ignore.stdout=TRUE, ignore.stderr = TRUE)
    outfile = "M.graphcem.dot.gif"
    list(src = outfile,
         height=300, width=400,
         alt = "CEM should be here")
  }, deleteFile = FALSE)
}
ui = fluidPage(
  fluidRow(column(6, imageOutput("cmPlot"))
           ,column(6, imageOutput("cemPlot"))
  ),
  fluidRow(column(6,  makeSliders()),
           column(3, "Community matrix", tableOutput("table")),
           column(3, "Effect matrix", tableOutput("effectMatrix"))
  ),
  fluidRow(column(3, ""), column(4, tableOutput("predictedEq"))),
  plotOutput("plot")
)

shinyApp(ui = ui, server = server)

