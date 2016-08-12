require(LevinsLoops)
require("LoopAnalyst")
require(shinyDebuggingPanel)
require(DOT)

data("cm.levins", package="LoopAnalyst")
rValues = reactiveValues(CM=cm.levins, CM_qual = cm.levins,
                         modelStringModified = FALSE,
                         constantsDefault=c(1000, rep( -200, 4)),
                         initialDefault=c(1000, rep(0, 4)),
                         constants=NULL, initial=NULL,
                         movingEqPlotFreeze = FALSE
)
modelStringList = c(
  'R -(R R )-> H H )-> x H )-> y y )-> y # Fig 2 Levins & Schultz 1996',
  'a -( a     a )-> b  #Simple prey-predator',
  'a -( a     a )-> b     b )-> c #two-level food chain',
  'a -( a     a )-> b     b )-> c c )-> d #three-level food chain',
  'a -( a     a )-> b     b )-> c c )-> d d )-> e # four-level food chain',
  'a -( a     a )-> b     b )-> p1     b )-> p2      p1 )-( p2 #Two predators, positive feedback',
  'x1 )-> x2  x2 )-( x3 x3 ->x1 x3 -( x3 # Levins 1974 fig3A ',
  'Qout-(Qout    Pressure-( Pressure    Depth-> Pressure  Pressure ->Qout  Qout-(Depth  ### Denver Dash bathtub',
  'Pt1 -( Pt1    Pt1 )-> Hv1    Hv1 )-> Pred
   Pt2 -( Pt2    Pt2 )-> Hv2    Hv2 )-> Pred
   Hv1 )-> Para
     # Levins 1974 Parasite Fig 5 '
    ####I -( Para   I -( Hv1   I -( Pred   I -( I
    ### I_cide -( Parasite   I_cide -( Hvore1   I_cide -( Pred   I_cide -( I_cide
    ###  But I_cide as a node doesnt work.
)

getParameterValue = function(parameter, CM) {
  TO = strsplit(parameter, "->")[[1]][2]
  FROM = strsplit(parameter, "->")[[1]][1]
  if(FROM == "external")  #is.a.constant(parameter)
    return(isolate(rValues$constants[TO]))
  else return(CM[TO, FROM])
}

nodeNameID = function(FROM=NULL, TO=NULL, paramLabel=NULL, tag=NULL) {
  if(!is.null(paramLabel)) {
    # if(is.a.constant(paramLabel))
    #     result = gsub(" (constant input)", "", paramLabel, fixed=TRUE)
    # else
      result = gsub("->", "_", paramLabel) ## convert from label to id
  }
  # else if(is.null(TO))
  #   result = FROM  ## create id for a constant from FROM only
  else
    result = paste(FROM, TO, sep="_")  ## create id for a link
  return(paste("Input", result, sep="_"))
}
nodeNameLabel = function(FROM, TO=NULL, tag=NULL) {
  # if(missing(TO))
  #   paste0(FROM, " (", tag, ")")
  # else
    paste(FROM, TO, sep="->")
}

is.a.constant = function(parameter)
  return(strsplit(parameter, split="->|_")[[1]][1] == "external")



server = function(input, output, session) {
  thisSession <<- session
  shinyDebuggingPanel::makeDebuggingPanelOutput(session)



  make.CM = reactive({
    ### responds to the slider values.
    rValues$CMsaved = rValues$CM
    rValues$nodeNames = nodeNames = rownames(rValues$CM_qual)
    CMtry = try({
      for(FROM in nodeNames)
        for(TO in nodeNames)
          rValues$CM[TO,FROM] = input[[nodeNameID(FROM,TO)]]
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
      for(NODE in rValues$nodeNames)
          rValues$constants[NODE] = input[[nodeNameID("external", NODE)]]
    })
  })
  make.initial = reactive({
    nSpecies = length(rValues$nodeNames)
    if (is.null(rValues$initial) | length(rValues$initial) != nSpecies)
      rValues$initialDefault = rValues$initial = c(1000,  rep(1, nSpecies-1))
    names(rValues$initialDefault) =  names(rValues$initial) = rValues$nodeNames
    try({
      for(NODE in rValues$nodeNames)
        rValues$initial[NODE] = input[[nodeNameID("initial", NODE)]]
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
    createParameterNumericInput = function(linkNum) {
      nodes = unlist(nameGrid[linkNum, ])
      node_TO = nodes[1]
      node_FROM = nodes[2]
      parameter = nodeNameLabel(node_FROM, node_TO)
      numericInput(inputId = nodeNameID(node_FROM, node_TO),
                   label = nodeNameLabel(node_FROM, node_TO),
                   min = -1.5, max = 1.5,
                   value = getParameterValue(parameter, CM),
                   step = 0.01)
    }
    returnVal = lapply(1:nrow(nameGrid), createParameterNumericInput)
    #### Turn the list of sliders into fluid rows. ####
    createAFluidRowForParameterInputs = function(sliders) fluidRow(
      lapply(returnVal[sliders], column, width=3)
    )
    returnVal = lapply(
      split(1:length(returnVal),
            1 + (-1 + 1:length(returnVal)) %% length(nodeNames)
      ), createAFluidRowForParameterInputs
    )  #shiny::tagAppendAttributes()
    #### Wrap the fluid rows into a conditionalPanel ####
    returnVal = div(style="background:darkGrey",
        checkboxInput(inputId='sliderPanelCheckbox', value=FALSE, width='100%',
                      label=em(strong("Show/hide editor for the CM (community matrix)"))),
        conditionalPanel('input.sliderPanelCheckbox', returnVal)
    )
    returnVal
  })
  ####  Create a renderUI for constants ####
  output$constants = renderUI({
    createConstantsNumericInput = function(nodeName) {
      numericInput(inputId = nodeNameID("external", nodeName),
                   label = nodeNameLabel("external", nodeName),
                   min = -1.5, max = 1.5,
                   value = rValues$constantsDefault[nodeName],
                   step = 0.01)
    }
    constantsInputs = lapply(rValues$nodeNames, createConstantsNumericInput)
    returnVal = fluidRow(column(2, h3("constants (inputs)")),
                         column(10, lapply(constantsInputs, column, width=2)))
    returnVal = div(style="background:darkGrey",
                    checkboxInput(inputId='constantsPanelCheckbox', value=FALSE, width='100%',
                                  label=em(strong("Show/hide editor for the constants (inputs)"))),
                    conditionalPanel('input.constantsPanelCheckbox', returnVal)
    )
    returnVal
  })

  #### Create a renderPlot for cmEquations ####
  output$cmEquations = renderPlot(height=function()360*length(rValues$nodeNames)/4, expr={
    makeEquationDisplay = function(cm){
      nodeNames = colnames(cm)
      nNodes = length(nodeNames)
      getRow = function(node){
        rhs = c(rValues$constants[node],
                paste(cm[node, ],'%*% bolditalic(',
                    nodeNames, ')', sep = ""  ) )
        rhs = paste(rhs, collapse = " + "   )
        lhs = paste("d*bolditalic(", node, ")/dt" )
        return(paste(lhs, rhs, sep = "=="))
      }
      formulas = sapply(nodeNames,getRow)

      par(fin=c(12, nNodes), mai=c(0,0,0,0)) # ,mai=c(0,0,0,0),
      plot(0:1, 0:1, axes=F, pch="", xlab="", ylab="")
        for(n in nNodes:1)
          text(0, 1-n/nNodes, parse(text=formulas[n]), adj = 0, cex=input$eqCex, xpd=NA)
    }
    makeEquationDisplay(rValues$CM)
  })

  ### Create a renderUI for equationPanel ####
  output$equationPanel = renderUI({
      returnVal = div(fluidRow(column(1, h3("Equations"),
                                      numericInput("eqCex", "fontsize", value = 2)),
                           column(11, plotOutput(outputId = "cmEquations"))),
                      hr())
      returnVal = div(style="background:darkGrey",
                      checkboxInput(inputId='equationPanelCheckbox', value=FALSE, width='100%',
                                    label=em(strong("Show/hide Differential Equations"))),
                      conditionalPanel('input.equationPanelCheckbox', returnVal)
    )
    returnVal
  })

  output$initial = renderUI({
    initialInputs = lapply(rValues$nodeNames,
                             function(nodeName) {
                               numericInput(inputId = nodeNameID("initial", nodeName),
                                            label = nodeNameLabel("initial", nodeName),
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
      for(NODE in names(newValues)) {
        initialInputID = nodeNameID("initial", NODE)
        updateNumericInput(session = session, inputId = initialInputID,
                           value = as.vector(newValues[NODE]) )
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
    input$Parameter
    val = as.vector(abs(getParameterValue(input$Parameter, rValues$CM)))
    if(val <= 2) val = 1
    updateNumericInput(session = session, inputId = "end_start",
                       min = -2*val , max = 2*val,
                       step = 0.1*val, value = 0.3*val)

  })

  observe({
    if(input$loadEquilibrium)
      isolate(loadNewInitials(rValues$predictedEq))
  })
  observe({
    if(input$loadDefault)
      isolate(loadNewInitials(rValues$initialDefault))
  })

  output$cmPlot = renderImage({
    dotFileCM = 'M.graphcm.dot'
    graph.cm(rValues$CM_qual, file=dotFileCM)
    ### Replace the "odot" circle for negative link by "tee" or "odiamond" or "invempty"
    changeArrows = F
    if (changeArrows) {
      system("sed s/odot/invempty/ > M.graphcm.fixed.dot < M.graphcm.dot")
      dotFileCM = 'M.graphcm.fixed.dot'
    }
    outfile = paste0(dotFileCM, ".svg")
    dot(DOT = paste(collapse=" ",
                    readLines(dotFileCM)),
        file=outfile)
    list(src = outfile,
         height=300, width=400,
         alt = "CM should be here")
  }, deleteFile = FALSE)


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
        updateNumericInput(session = session, nodeNameID(paramLabel=input$Parameter),
                                                    value = as.vector(newValue)) #JSON
        cat("input$Load_end:  startingValue=", startingValue, "  newValue=", newValue, "\n")
        TO = strsplit(input$Parameter, "->")[[1]][2]
        FROM = strsplit(input$Parameter, "->")[[1]][1]
        if(FROM == "external")   ### is.a.constant(input$Parameter)
          rValues$constants[input$Parameter] <- newValue
        else
          rValues$CM[TO, FROM] = newValue
        })
    }
  })

  output$cemPlot = renderImage({
    CEM = make.cem(rValues$CM_qual)
    CEM = t(CEM) ### Correction
    dotFileCEM = 'M.graphcem.dot'
    graph.cem(CEM, file=dotFileCEM)
    changeArrows = F
    if (changeArrows) {
      system("sed s/odot/invempty/ > M.graphcem.fixed.dot < M.graphcem.dot")
      dotFileCEM = 'M.graphcem.fixed.dot'
    }
    dot(DOT = paste(collapse=" ",
                    readLines(dotFileCEM)),
        file=paste0(dotFileCEM, ".svg"))
    outfile = paste0(dotFileCEM, ".svg")
    list(src = outfile,
         height=300, width=400,
         alt = "CEM should be here")
  }, deleteFile = FALSE)
  observe({
    parameter_names = c(
      nodeNameLabel(rValues$nameGrid[[1]], rValues$nameGrid[[2]])
    )
    constant_names = nodeNameLabel("external", rValues$nameGrid[[2]])
    updateSelectInput(session = session, inputId = "Parameter",
                      choices = c(constant_names, parameter_names))
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
                            label = "model string", value = ""
                  )),
           column(6,
                  textInput(inputId = "IpmnetString",
                            width="800px",
                            label = HTML("For pasting at <a href=http://ipmnet.org/loop/loopanalysis.aspx> Ipmnet.org</a>"),
                            value = ""
                  ))
           ),
  fluidRow(column(6,
                  imageOutput("cmPlot"),
                  h2("Community matrix"),
                  tagAppendAttributes(style="font-size:200%",
                                      tableOutput("cmMatrix")))
           ,column(6,
                   imageOutput("cemPlot"),
                   h2("Effect matrix"),
                   tagAppendAttributes(style="font-size:200%",
                                       tableOutput("effectMatrix")))
  ),
  tagAppendAttributes(style="border-width:10px", hr()),
  fluidRow(column(offset = 0, 12,  uiOutput("sliders"))),
  uiOutput("constants"), # constant inputs into nodes
  uiOutput("equationPanel"),
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

