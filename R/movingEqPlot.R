movingEqPlot = function(CM,
                    initial,
                    paramToChange,
                    start,
                    end,
                    nPoints = 10,
                    constants, death=-100,
                    plot=TRUE, print=FALSE,
                    returnLast=FALSE,
                    attachAttributes=FALSE) {
  if(missing(CM) | is.null(CM)) {
    if(length(find("cm.levins")) == 0)
      data(cm.levins)
    CM = cm.levins
  }
  M = CM
  nSpecies = nrow(M)
  if(is.null(rownames(M))) rownames(M) = 1:nSpecies
  if(is.null(colnames(M))) colnames(M) = 1:nSpecies
  if(missing(initial)) {
    initial = c(1000, rep(1, nSpecies-1))
    names(initial) = rownames(M)
  }
  if(missing(constants))
    constants = c(1000,  rep(death, nSpecies-1))
  trajectory = matrix(NA, nrow = nPoints, ncol=ncol(M))
  colnames(trajectory) = colnames(M)
  dimnames(trajectory)[[2]] = colnames(M)
  trajectory[1, ] = initial
  nodes = strsplit(paramToChange, "->")[[1]]
  fromNode = nodes[1]
  toNode = nodes[2]
  increment = (end-start)/nPoints
  for (t in 2:nPoints) {
    CM[toNode, fromNode] = CM[toNode, fromNode] + increment
    trajectory[t, ] =
      solve ( CM, -constants)
  }
   {
    timeline = seq(start,end, length=nPoints)
    plot(timeline, trajectory[ , 1], pch="",
         ylim = range(c(trajectory)),
         xlab=paramToChange,
         ylab="trajectories",
         main="Moving Equilibria")
    for(species in 1:nSpecies)
      lines(timeline, trajectory[ , species], col=species)
    legend("topright", rownames(M), text.col=1:nSpecies, horiz = TRUE)
  }
  if(returnLast)  # Last
    returnVal = trajectory[length(timeline), ]
  else returnVal = trajectory
  if(print) return(returnVal)
  return(returnVal)
}

