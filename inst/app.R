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
  'x1 )-> x2  x2 )-( x3 x3 ->x1 x3 -( x3 # Levins 1974 fig3A ',
  'Qout-(Qout    Pressure-( Pressure    Depth-> Pressure  Pressure ->Qout  Qout-(Depth  ### Denver Dash bathtub'
)


nodeNameID = function(n1=NULL, n2=NULL, param=NULL) {
  if(!is.null(param))
    return(paste("Input", gsub("->", "_", param), sep="_"))
  else if(is.null(n2))
    return(paste("Input", n1, sep="_"))
  else
    return(paste("Input", n1, n2, sep="_"))
}
nodeNameLabel = function(n1, n2=NULL) {
  if(missing(n2))
    paste(n1, sep="->")
  else
    paste(n1, n2, sep="->")
}


server = function(input, output, session) {
  thisSession <<- session
  shinyDebuggingPanel::makeDebuggingPanelOutput(session)

  rValues = reactiveValues(CM=cm.levins, CM_qual = cm.levins,

                           modelStringModified = FALSE,
                           constantsDefault=c(1000, rep( -200, 4)),
                           initialDefault=c(1000, rep(0, 4)),
                           constants=NULL, initial=NULL,
                           movingEqPlotFreeze = FALSE
  )

  make.CM = reactive({
    ### responds to the slider values.
    rValues$CMsaved = rValues$CM
    rValues$nodeNames = nodeNames = rownames(rValues$CM_qual)
    CMtry = try({
      for(X1 in nodeNames)
        for(X2 in nodeNames)
          rValues$CM[X2,X1] = input[[nodeNameID(X1,X2)]]
    })
    if(class(CMtry) == 'try-error'
       | is.null(CMtry))
      returnVal = rValues$CM
    else returnVal = rValues$CM = CMtry
    make.initial()
    make.constants()
    return (returnVal)
  })
  make.constants = reactive({
    death = (-200)
    nSpecies = length(rValues$nodeNames)
    if (is.null(rValues$constants) | length(rValues$constants) != nSpecies)
      rValues$constantsDefault = rValues$constants = c(1000,  rep(death, nSpecies-1))
    names(rValues$constantsDefault) =  names(rValues$constants) = rValues$nodeNames
    try({
      for(X1 in rValues$nodeNames)
          rValues$constants[X1] = input[[paste0("constants_", nodeNameID(X1))]]
    })
  })
  make.initial = reactive({
    nSpecies = length(rValues$nodeNames)
    if (is.null(rValues$initial) | length(rValues$initial) != nSpecies)
      rValues$initialDefault = rValues$initial = c(1000,  rep(1, nSpecies-1))
    names(rValues$initialDefault) =  names(rValues$initial) = rValues$nodeNames
    try({
      for(X1 in rValues$nodeNames)
        rValues$initial[X1] = input[[paste0("initial_",nodeNameID(X1))]]
    })
    cat('make.initial: '); print(rValues$initial)
  })
  observe({
    updateTextInput(session=session, inputId = "modelString",
                    value = input$modelList,
                    label = paste("Selected model string ",
                                  gsub(".*#", "", input$modelList)))
  })
  observe(priority = 1, {
    input$modelString ## Reactivity only to input$modelString
    isolate({
      rValues$comment = gsub(".*#", "", input$modelList)
      rValues$modelStringModified <-
        !identical(input$modelString, input$modelList)
      updateTextInput(session=session, inputId = "modelString",
                      label =HTML(
                        ifelse(rValues$modelStringModified,
                               "Modified model string",
                               paste("Selected model string (",
                                     rValues$comment, ")") )
                        )
      )
      if(!is.null(input$modelString) & input$modelString != "") {
        tryResult = try( {
          stringToCM(input$modelString)
        })
        if(class(tryResult) != 'try-error')
          rValues$CM <-rValues$CM_qual <- tryResult
        else cat("Error in stringToCM:  ", tryResult, "\n")
      }
    })
  })

  observe({
    updateTextInput(session = session,inputId = "IpmnetString",
                    value = try(CMtoIPMnet(stringToCM(input$modelString))))
  })

  output$cmMatrix = renderTable({
    try_out = try(out.cm(rValues$CM_qual))
  })
  output$effectMatrix = renderTable({
    cat("CEM: effectMatrix\n")
    print(out.cm(t(attr(rValues$dynamSimResult, "effectMatrix"))))
  })
  output$sliders = renderUI( {
    CM = rValues$CM_qual
    rValues$nodeNames = nodeNames = rownames(CM)
    rValues$nameGrid = nameGrid = expand.grid(rownames(CM), rownames(CM),
                                              stringsAsFactors = FALSE)
    parameterList = apply(nameGrid, 1, paste0, collapse="->")
    returnVal = lapply(1:nrow(nameGrid),
                       function(linkNum) {
                         nodes = unlist(nameGrid[linkNum, ])
                         node_to = nodes[1]
                         node_from = nodes[2]
                         parameter = parameterList[linkNum]
                         numericInput(inputId = nodeNameID(node_from, node_to),
                                      label = nodeNameLabel(node_from, node_to),
                                      min = -1.5, max = 1.5,
                                      value = getParameterValue(parameter, CM),
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
    returnVal = div(style="background:darkGrey",
        checkboxInput(inputId='sliderPanelCheckbox', value=FALSE, width='100%',
                      label=em(strong("Show/hide editor for the CM (community matrix)"))),
        conditionalPanel('input.sliderPanelCheckbox', returnVal)
    )
    returnVal
  })
  output$constants = renderUI({
    constantsInputs = lapply(rValues$nodeNames,
                       function(nodeName) {
                         numericInput(inputId = paste0("constants_", nodeNameID(nodeName)),
                                      label = nodeNameLabel(nodeName),
                                      min = -1.5, max = 1.5,
                                      value = rValues$constantsDefault[nodeName],
                                      step = 0.01)
                       }
    )
    returnVal = fluidRow(column(2, h3("constants (inputs)")),
                         column(10, lapply(constantsInputs, column, width=2)))
    returnVal = div(style="background:darkGrey",
                    checkboxInput(inputId='constantsPanelCheckbox', value=FALSE, width='100%',
                                  label=em(strong("Show/hide editor for the constants (inputs)"))),
                    conditionalPanel('input.constantsPanelCheckbox', returnVal)
    )
    returnVal
  })

  output$matrix = renderUI({
    getRow = function(irow){
      rhs = paste(rValues$CM[irow, ],'*',rValues$nodeNames, sep = "" , collapse = " + "   )
      lhs = paste("dot(",rValues$nodeNames[irow] )
      return(paste(rhs, lhs, sep = "=="))


    }
    cmEquations = lapply(rValues$CM,
                        paste, collapse = "")
    returnVal = fluidRow(column(3, h3("Equations")),
                            column(10, lapply(cmEquations,column, width =2)))
    returnVal = div(style="background:darkGrey",
                      checkboxInput(inputId='matrixPanelCheckbox', value=FALSE, width='100%',
                                    label=em(strong("Show/hide Differential Equations"))),
                      conditionalPanel('input.matrixPanelCheckbox', returnVal)
    )
    returnVal
  })



  output$initial = renderUI({
    initialInputs = lapply(rValues$nodeNames,
                             function(nodeName) {
                               numericInput(inputId = paste0("initial_",nodeNameID(nodeName)),
                                            label = nodeNameLabel(nodeName),
                                            min = -1.5, max = 1.5,
                                            value = rValues$initialDefault[nodeName],
                                            step = 0.01)
                             }
    )
    returnVal = fluidRow(column(offset=1, 11, lapply(initialInputs, column, width=3)))
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
    # abline(h=dynamSimResult)
    # if(exists("previousdynamSimResult"))
    #   abline(h=previousdynamSimResult, lty=2)
    previousdynamSimResult <<- dynamSimResult
  })

  loadNewInitials = function(newValues) {
    try({
      for(X1 in names(newValues)) {
        initialInputID = paste0("initial_", nodeNameID(X1))
        updateNumericInput(session = session, inputId = initialInputID,
                           value = as.vector(newValues[X1]) )
      }
      # If you do not wrap the value in "as.vector", then it is a named vector,
      # and we get the error
      #   Input to asJSON(keep_vec_names=TRUE) is a named vector.
      #   In a future version of jsonlite, this option will not be supported,
      #   and named vectors will be translated into arrays instead of objects.
      #   If you want JSON object output, please use a named list instead. See ?toJSON.
      # Guess what. It already bombs.
      # "as.vector" solves the problem by stripping the name off of the value.
    })
  }
  observe({
    if(input$loadEquilibrium)
      isolate(loadNewInitials(rValues$predictedEq))
  })
  observe({
    if(input$loadDefault)
      isolate(loadNewInitials(rValues$initialDefault))
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

  getParameterValue = function(parameter, CM) {
    from = strsplit(parameter, "->")[[1]][2]
    to = strsplit(parameter, "->")[[1]][1]
    return(CM[to, from])
  }
  output$movingEqPlot = renderPlot({

    if(rValues$movingEqPlotFreeze)
      CM = isolate({rValues$CMsaved})
    else
      CM <- rValues$CM
    #isolate(rValues$movingEqPlotFreeze <- FALSE)

    end_start = input$end_start
    start = getParameterValue(input$Parameter, CM)
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
        ### For communicating and 'freezing' the movingEq plot, where CMsaved will be used
        ### in order to preserve the current plot.
        rValues$CMsaved <- rValues$CM
        rValues$movingEqPlotFreeze = TRUE
        ### Load the current equilibrium as the starting value for the dynamic plot.
        loadNewInitials(rValues$predictedEq)
        # Finally update the CM with the "end" value of the parameter that is changing.
        increment = input$end_start
        startingValue = getParameterValue(input$Parameter, rValues$CM)
        newValue = startingValue + increment
        updateNumericInput(session = session, nodeNameID(param=input$Parameter),
                                                         value = newValue)
        cat("input$Load_end:  startingValue=", startingValue, "  newValue=", newValue, "\n")
        rValues$CM[strsplit(input$Parameter, "->")[[1]][2],
                   strsplit(input$Parameter, "->")[[1]][1]] <- newValue
        print(rValues$CM)
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
#
#   observe({
#     if(!is.null(input$loadStringIntoModel))
#       if(input$loadStringIntoModel > 0){
#         isolate(rValues$CM <-rValues$CM_qual <- stringToCM(input$modelString))
#       }
#     if(!is.null(input$loadString))
#       if(input$loadString > 0){
#         isolate(rValues$CM <-rValues$CM_qual <- stringToCM(input$modelString))
#       }
#   })
#   output$comment = renderText({rValues$comment})
      }

ui = fluidPage(
  shinyDebuggingPanel::withDebuggingPanel(),
  h1(style="font-color:black; text-align:center", "Dynamic Model Explorer"),
  hr(),
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
                  ))
           ),
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
  fluidRow(column(offset = 0, 12,  uiOutput("sliders"))),
  uiOutput("constants"), # constant inputs into nodes
  uiOutput("matrix"),
  fluidRow(column(3, ""), column(4, tableOutput("equilibriumTable"))),
  fluidRow(column(6, h2("Dynamic trajectory plot"),
                  fluidRow(
                    numericInput(inputId = "Tmax",label = "Tmax",value = 15, min = 1, step = 1 ),
                    checkboxInput("noNeg","negatives disallowed?", value = TRUE )),
                  h4("Initial values: "),
                  fluidRow(column(offset=1, 3, actionButton("loadEquilibrium", "Load Equilibrium")),
                           column(3, actionButton("loadDefault", "Load Default Values"))),
                  uiOutput("initial"),  # initial values of variables
                  plotOutput("plot")),
           column(6, h2("Moving equilibrium plot"),
                  fluidRow(
                    column(6, selectInput(inputId = "Parameter",label = "Parameter to Change", choices = "R->R")),
                    column(6, numericInput(inputId = "end_start", label = "end minus start", min = -2 ,max = 2, step = 0.1, value = 0.1))
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

