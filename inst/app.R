library(LevinsLoops)
library("LoopAnalyst")
library(shinyDebuggingPanel)

data("cm.levins", package="LoopAnalyst")

modelStringList = c(
  'R )-> H H )-> x H )-> y y )-> y # Fig 2 Levins & Schultz 1996',
   'a -( a     a )-> b  #Simple prey-predator',
   'a -( a     a )-> b     b )-> c #two-level food chain',
   'a -( a     a )-> b     b )-> c c )-> d #three-level food chain',
   'a -( a     a )-> b     b )-> c c )-> d d )-> e # four-level food chain',
  'a -( a     a )-> b     b )-> p1     b )-> p2      p1 )-( p2 #Two predators, positive feedback'
)

nodeNameID = function(n1, n2) paste("Input", n1, n2, sep="_")
nodeNameLabel = function(n1, n2) paste(n1, n2, sep="->")


server = function(input, output, session) {
  thisSession <<- session
  shinyDebuggingPanel::makeDebuggingPanelOutput(session)

  rValues = reactiveValues(CM=cm.levins, CM_qual = cm.levins,
                           constants=c(1000,0,0,0))

  make.CM = reactive({
    ### responds to the slider values.
    nodeNames = rownames(rValues$CM_qual)
    CMtry = try({
      for(X1 in nodeNames)
        for(X2 in nodeNames)
          rValues$CM[X2,X1] = input[[nodeNameID(X1,X2)]]
    })
    print(rValues$CM)
    print(CMtry)
    print(class(CMtry))
    if(class(CMtry) == 'try-error'
       | is.null(CMtry))
      returnVal = rValues$CM
    else returnVal = rValues$CM = CMtry
    nSpecies = length(nodeNames)
    death = 0
    rValues$constants = c(1000,  rep(death, nSpecies-1))

    return (returnVal)
  })
  observe({
    updateTextInput(session=session, inputId = "modelString",
                    value = input$modelList)
    rValues$comment = gsub(".*#", "", input$modelList)

  })
  observe({
    updateTextInput(session = session,inputId = "IpmnetString",
                    value = try(CMtoIPMnet(stringToCM(input$modelString))))
  })

  output$cmMatrix = renderTable({
    try_out = try(out.cm(rValues$CM_qual))
  })
  output$effectMatrix = renderTable({
    cat("effectMatrix\n")
    print(out.cm(t(attr(rValues$dynamSimResult, "effectMatrix"))))
  })
  output$sliders = renderUI( {
    CM = rValues$CM_qual
    rValues$nodeNames = nodeNames = rownames(CM)
    rValues$nameGrid = nameGrid = expand.grid(rownames(CM), rownames(CM),
                           stringsAsFactors = FALSE)
    returnVal = lapply(1:nrow(nameGrid),
                       function(linkNum) {
                         nodes = unlist(nameGrid[linkNum, ])
                         node_to = nodes[1]
                         node_from = nodes[2]
                         numericInput(inputId = nodeNameID(node_from, node_to),
                                      label = nodeNameLabel(node_from, node_to),
                                      min = -1.5, max = 1.5,
                                      value = CM[node_to, node_from],
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
  })

  output$predictedEq = renderTable({
    cat("predictedEq:\n")
    predictedEq = attr(rValues$dynamSimResult, "predictedEq")
    predictedEq =  as.data.frame(as.list(predictedEq))
    predictedEq = rbind(predictedEq, rValues$dynamSimResult)
    rownames(predictedEq) = c("predicted equilibrium", 'final in simulation')
    print(predictedEq)
  })
  output$plot = renderPlot({
    library(LevinsLoops)
    dynamSimResult = rValues$dynamSimResult =
      dynamSim(M = make.CM(), attachAttributes=TRUE, returnLast=TRUE,noNeg = input$noNeg, Tmax = input$Tmax)
    abline(h=dynamSimResult)
    if(exists("previousdynamSimResult"))
      abline(h=previousdynamSimResult, lty=2)
    previousdynamSimResult <<- dynamSimResult
  })


  output$cmPlot = renderImage({
    graph.cm(rValues$CM_qual, file="M.graphcm.dot")
    ### Replace the "odot" circle for negative link by "tee" or "odiamond" or "invempty"
    system("sed s/odot/invempty/ > M.graphcm.fixed.dot < M.graphcm.dot")
    system("dot -Tgif -O M.graphcm.fixed.dot",
           ignore.stdout=TRUE, ignore.stderr = TRUE)
    outfile = "M.graphcm.fixed.dot.gif"
    list(src = outfile,
         height=300, width=400,
         alt = "CM should be here")
  }, deleteFile = FALSE)


  output$movingEqPlot = renderPlot({
    browser(text = "movingEqPlot")
    end_start = input$end_start
    start = input[[paste0("Input_", gsub("->", "_", input$Parameter))]]
    end = start + end_start
  movingEqPlot(rValues$CM,
                 initial = solve(rValues$CM, -rValues$constants),
                 paramToChange = input$Parameter,
                 constants = rValues$constants,
                 start = start,
                 end = end)
  })

  output$cemPlot = renderImage({
    CEM = make.cem(rValues$CM_qual)
    CEM = t(CEM) ### Correction
    graph.cem(CEM, file="M.graphcem.dot")
    system("sed s/odot/invempty/ > M.graphcem.fixed.dot < M.graphcem.dot")
    system("dot -Tgif -O M.graphcem.fixed.dot",
           ignore.stdout=TRUE, ignore.stderr = TRUE)
    outfile = "M.graphcem.fixed.dot.gif"
    list(src = outfile,
         height=300, width=400,
         alt = "CEM should be here")
  }, deleteFile = FALSE)
  observe({
    parameter_names = nodeNameLabel(rValues$nameGrid[[1]], rValues$nameGrid[[2]])
    updateSelectInput(session = session, inputId = "Parameter", choices = parameter_names)
  })

  observe({
    if(!is.null(input$loadModel))
      if(input$loadModel > 0){
        isolate(rValues$CM <-rValues$CM_qual <- stringToCM(input$modelString))
      }
  })
  output$comment = renderText({rValues$comment})
}

ui = fluidPage(
  shinyDebuggingPanel::withDebuggingPanel(),
  fluidRow(column(4, selectInput(inputId = "modelList", "some models",
                                 modelStringList)),
           column(6,
                  textInput(inputId = "modelString",
                            width="800px",
                            label = "model string (either Pittsburgh-style or ipm-style)", value = ""
                  )),
           column(6,
                  textInput(inputId = "IpmnetString",
                            width="800px",
                            label = "Ipm", value = ""
                  )),

           column(2, br(),
                  actionButton("loadModel", "Load model"))),

  fluidRow(column(12, tagAppendAttributes
                  (h2(textOutput("comment")), style="text-align:center"))),
  fluidRow(column(6, h2("Community matrix"),
                  imageOutput("cmPlot"),
                  tagAppendAttributes(style="font-size:200%",
                                      tableOutput("cmMatrix")))
           ,column(6, h2("Effect matrix"),
                   imageOutput("cemPlot"),
                   tagAppendAttributes(style="font-size:200%",
                                       tableOutput("effectMatrix")))
  ),
  tagAppendAttributes(style="border-width:10px", hr()),
  fluidRow(column(offset = 2, 6,  uiOutput("sliders"))),
  fluidRow(column(3, ""), column(4, tableOutput("predictedEq"))),
  fluidRow(column(6, h2("Dynamic"), numericInput(inputId = "Tmax",label = "Tmax",value = 15, min = 1, step = 1 ),
                  checkboxInput("noNeg","negatives disallowed?", value = TRUE ),
                  plotOutput("plot")),
           column(6, h2("MOVING EQUILIBRIUM PLOT will go here"),
                  selectInput(inputId = "Parameter",label = "Parameter to Change", choices = "K"),
                  numericInput(inputId = "end_start", label = "end-start", min = -2 ,max = 2, step = 0.1, value = 1),
                  plotOutput("movingEqPlot")
  )

  )
)

shinyApp(ui = ui, server = server)

