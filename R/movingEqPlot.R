movingEqPlot = function(CM,
                    paramToChange,
                    start,
                    end,
                    nPoints = 10,
                    constants, death=-100,
                    attachAttributes=FALSE,
                    test=FALSE) {
  if(test) {
    if(missing(paramToChange))
      paramToChange = "R->R"
    constants = c(1000, rep(death, nrow(cm.levins)-1))
    return(
      movingEqPlot(CM=cm.levins,
                paramToChange=paramToChange,
                start=cm.levins[1,1],
                end=cm.levins[1,1]+0.1,
                constants=constants)
    )
  }
  if(missing(CM) | is.null(CM)) {
    if(length(find("cm.levins")) == 0)
      data(cm.levins)
    CM = cm.levins
  }
  M = CM
  nSpecies = nrow(M)
  if(is.null(rownames(M))) rownames(M) = 1:nSpecies
  if(is.null(colnames(M))) colnames(M) = 1:nSpecies
  if(missing(constants))
    constants = c(1000,  rep(death, nSpecies-1))
  trajectory = matrix(NA, nrow = nPoints, ncol=ncol(M))
  colnames(trajectory) = colnames(M)
  dimnames(trajectory)[[2]] = colnames(M)
  trajectory[1, ] = solve(CM, -constants)

  nodes = strsplit(paramToChange, "->")[[1]]
  fromNode = nodes[1]
  toNode = nodes[2]
  increment = (end-start)/nPoints
  for (t in 2:nPoints) {
    CM[toNode, fromNode] = CM[toNode, fromNode] + increment
    trajectory[t, ] =
      solve ( CM, -constants)
  }
  timeline = seq(start,end, length=nPoints)
  plot(timeline, trajectory[ , 1], pch="",
       ylim = range(c(trajectory)),
       xlab=paramToChange,
       ylab="trajectories",
       main="Moving Equilibria")
  for(species in 1:nSpecies)
    lines(timeline, trajectory[ , species], col=species)
  ## Compare the CEM predictions to the changes, quantitatively.
  CEMchanges = (-1)*solve(CM) [ , toNode]
  ### Note that the "toNode" for the parameter (CM) is the one directly affected,
  ### so in the CEM it is the "fromNode", the column,
  ### and the "toNodes" are the ones indirectly affected.
  observedChanges = trajectory[nPoints, ] - trajectory[1, ]
  changes = rbind(CEMchanges, observedChanges)
  rownames(changes) = c("predicted", "observed")
  colnames(changes) = rownames(CM)
  changes = round(changes, 2)
  # rightJustify = function(n, digits=3) {
  #   ns = as.character(round(n, digits))
  #   return(paste(rep(" ", )))s
  # }
  legend("left", rownames(M),
         text.col=1:nSpecies, horiz = FALSE)
  library(plotrix)
  try(
    plotrix::addtable2plot(display.rownames = TRUE,
                start+(end-start)*0.1,
                mean(c(min(trajectory), max(trajectory))),
                changes,
                xpad=1, ypad=1)
  )
  return(changes)
}

