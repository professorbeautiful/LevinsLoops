library(LevinsLoops)
library("LoopAnalyst")
library(shinyDebuggingPanel)

data("cm.levins", package="LoopAnalyst")

modelStringList = c(
  'R -(R R )-> H H )-> x H )-> y y )-> y # Fig 2 Levins & Schultz 1996',
  'a -( a     a )-> b  #Simple prey-predator',
  'a -( a     a )-> b     b )-> c #two-level food chain',
  'a -( a     a )-> b     b )-> c c )-> d #three-level food chain',
  'a -( a     a )-> b     b )-> c c )-> d d )-> e # four-level food chain',
  'a -( a     a )-> b     b )-> p1     b )-> p2      p1 )-( p2 #Two predators, positive feedback',
  'x1 )-> x2  x2 )-( x3 x3 ->x1 x3 -( x3 # Levins 1974 fig3A '
)

nodeNameID = function(n1, n2) paste("Input", n1, n2, sep="_")
nodeNameLabel = function(n1, n2) paste(n1, n2, sep="->")


server = function(input, output, session) {
  thisSession <<- session
  shinyDebuggingPanel::makeDebuggingPanelOutput(session)

  rValues = reactiveValues(CM=cm.levins, CM_qual = cm.levins,
                           movingEqPlotFreeze = FALSE
  )

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
    death = (-100)
    if (is.null(rValues$initial) | length(rValues$initial) != nSpecies){

      rValues$constants = c(1000,  rep(death, nSpecies-1))
      rValues$initial = c(1000,  rep(1, nSpecies-1))
    }
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

  output$equilibriumTable = renderTable({
    cat("predictedEq:\n")
    predictedEq = rValues$predictedEq = attr(rValues$dynamSimResult, "predictedEq")
    equilibriumTable =  as.data.frame(as.list(predictedEq))
    equilibriumTable = rbind(equilibriumTable, rValues$dynamSimResult)
    rownames(equilibriumTable) = c("predicted equilibrium", 'final in simulation')
    print(equilibriumTable)
  })
  output$plot = renderPlot({
    library(LevinsLoops)
    dynamSimResult = rValues$dynamSimResult =
      dynamSim(M = make.CM(),
               constants=rValues$constants,
               initial=rValues$initial,
               attachAttributes=TRUE, returnLast=TRUE,
               noNeg = input$noNeg, Tmax = input$Tmax)
    abline(h=dynamSimResult)
    if(exists("previousdynamSimResult"))
      abline(h=previousdynamSimResult, lty=2)
    previousdynamSimResult <<- dynamSimResult
  })


  observe({
    if(input$loadEquilibrium){
      isolate(rValues$initial <- rValues$predictedEq)
    }
  })
  observe({
    if(input$loadDefault){
      nSpecies = nrow(rValues$CM_qual)
      isolate(rValues$initial <- c(1000,  rep(1, nSpecies-1)))
    }
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

    if(rValues$movingEqPlotFreeze)
      CM = rValues$CMsaved
    else
      CM <- rValues$CM
    rValues$movingEqPlotFreeze = FALSE

    end_start = input$end_start
    start = input[[paste0("Input_", gsub("->", "_", input$Parameter))]]
    end = start + end_start
    return(movingEqPlot(CM = CM,
                        paramToChange = input$Parameter,
                        constants = rValues$constants,
                        start = start,
                        end = end)
    )
  })

  observe({
    if(input$Load_end){
      isolate({
        rValues$CMsaved <- rValues$CM
        rValues$movingEqPlotFreeze = TRUE
        rValues$initial <- rValues$predictedEq
        print(rValues$initial)
        nodes = strsplit(input$Parameter, "->")[[1]]
        fromNode = nodes[1]
        toNode = nodes[2]
        increment = input$end_start
        newValue = rValues$CM[toNode, fromNode] + increment
        rValues$CM[toNode, fromNode] = newValue
        updateNumericInput(session = session, nodeNameID(n1 = toNode, n2 = fromNode),
                                                         value = newValue)
        })
    }

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
  fluidRow(column(3, ""), column(4, tableOutput("equilibriumTable"))),
  fluidRow(column(6, h2("Dynamic"),
                  fluidRow(
                    numericInput(inputId = "Tmax",label = "Tmax",value = 15, min = 1, step = 1 ),
                    checkboxInput("noNeg","negatives disallowed?", value = TRUE )),
                  "Starting Value: ",
                  actionButton("loadEquilibrium", "Load Equilibrium"),
                  actionButton("loadDefault", "Load Default Values"),
                  plotOutput("plot")),
           column(6, h2("MOVING EQUILIBRIUM PLOT will go here"),
                  fluidRow(
                    column(6, selectInput(inputId = "Parameter",label = "Parameter to Change", choices = "R->R")),
                    column(6, numericInput(inputId = "end_start", label = "end minus start", min = -2 ,max = 2, step = 0.1, value = 1))
                  ),
                  fluidRow(column(12, offset = 6 ,tagAppendAttributes
                    (actionButton(inputId = "Load_end", label = "Load End Value into CM")
                    ))),


                  plotOutput("movingEqPlot")


              #add end to CM Matrix
              #freeze moving equilibrium
              #find where the cem is originally made and duplicate it by setting equal to rvalues$CMsaved
           )
  )
)

shinyApp(ui = ui, server = server)

