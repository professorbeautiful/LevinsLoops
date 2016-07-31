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

  increment = (end-start)/nPoints
  for (t in 2:nPoints) {
    TO = strsplit(paramToChange, "->")[[1]][2]
    FROM = strsplit(paramToChange, "->")[[1]][1]
    if(FROM == "external") {  ### is.a.constant(paramToChange)
      constants[TO] = constants[TO] + increment
    }
    else {
      CM[TO, FROM] = CM[TO, FROM] + increment
    }
    trajectory[t, ] =
      solve ( CM, -constants)
  }
  timeline = seq(start,end, length=nPoints)
  plot(timeline, trajectory[ , 1], pch="",
       ylim = range(c(trajectory)),
       xlim = c(start, end),
       xlab=paramToChange,
       ylab="trajectories",
       main="")
  for(species in 1:nSpecies)
    lines(timeline, trajectory[ , species], col=species)
  ## Compare the CEM predictions to the changes, quantitatively.
  CEMchanges = (-1) * sign(end-start) * solve(CM) [ , TO]
  ### Note that the "toNode" for the parameter (CM) is the one directly affected,
  ### so in the CEM it is the "fromNode", the column,
  ### and the "toNodes" are the ones indirectly affected.
  observedChanges = trajectory[nPoints, ] - trajectory[1, ]
  changes = rbind(sign(CEMchanges), observedChanges)
  rownames(changes) = c("predicted changes", "observed changes")
  colnames(changes) = rownames(CM)
  changes = round(changes, 2)
  # rightJustify = function(n, digits=3) {
  #   ns = as.character(round(n, digits))
  #   return(paste(rep(" ", )))s
  # }
  mtext(text="START", at=start, side = 1, line = 2)
  mtext(text="END", at=end, side = 1, line = 2)
  legend(x = par()$usr[1], y = par()$usr[4], legend = rownames(M), yjust = 0, xpd = NA,
         text.col=1:nSpecies, horiz = TRUE)
  try(
    if(start < end)
      plotrix::addtable2plot(display.rownames = TRUE,
                             x = start+(end-start)*0.1,
                             y = mean(c(min(trajectory), max(trajectory))),
                             table = changes,
                             xpad=1, ypad=1)
  )
  #plotrix doessn't work well if x axis is in reverse direction.

  return(changes)
}

