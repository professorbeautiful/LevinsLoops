dynamSim = function(M,
                    initial, values,
                    constants, death=-100,
                    Tmax=15, timestep = 0.01,
                    plot=TRUE, print=FALSE,
                    findEq=TRUE,
                    returnLast=FALSE,
                    noNeg=TRUE,
                    attachAttributes=FALSE) {
  if(missing(M)) {
    if(length(find("cm.levins")) == 0)
       data(cm.levins)
    M = cm.levins
  }
  nSpecies = nrow(M)
  if(is.null(rownames(M))) rownames(M) = 1:nSpecies
  if(is.null(colnames(M))) colnames(M) = 1:nSpecies
  if(missing(initial) | is.null(initial)) {
    initial = c(1000, rep(1, nSpecies-1))
    names(initial) = rownames(M)
  }
  if(missing(values)) {
    values = M
  }
  if(missing(constants))
    constants = c(1000,  rep(death, nSpecies-1))
  ntimes = ceiling(Tmax/timestep)
  trajectory = matrix(NA, nrow = ntimes, ncol=ncol(M))
  colnames(trajectory) = colnames(M)
  trajectory[1, ] = initial
  for (t in 2:ntimes) {
    trajectory[t, ] = trajectory[t-1, ] +
      timestep * constants +
      timestep * trajectory[t-1, ] %*% t(values)
    if(noNeg)
      trajectory[t, ] = pmax(0, trajectory[t, ])
  }
  if(plot) {
    timeline = seq(0,Tmax, by=timestep)[-1]
    plot(timeline, trajectory[ , 1], pch="",
         ylim = range(c(trajectory)),
         ylab="trajectories",
         main="Dynamics")
    for(species in 1:nSpecies)
      lines(timeline, trajectory[ , species], col=species)
    legend("topright", rownames(M), text.col=1:nSpecies, horiz = TRUE)
  }
  if(findEq) {
    cat("predictedEq:\n")
    predictedEq = print(solve(values, -constants))
  }
  if(returnLast)  # Last
    returnVal = trajectory[length(timeline), ]
  else returnVal = trajectory
  if(attachAttributes) {
    `attr<-`(returnVal, "predictedEq", predictedEq)
    `attr<-`(returnVal, "effectMatrix", LoopAnalyst::make.cem(M))  ### too much time!
    #`attr<-`(returnVal, "effectMatrix", 1:4)
  }
  if(print) return(returnVal)
  return(returnVal)
}
modifyValues = function(M, from, to, increment) {
  M[to, from ] = M[to, from ] + increment
  return (M)
}
options(digits=3)
revRows = function(M) M[nrow(M):1, ]
if(interactive())
  print(revRows(dynamSim())[1,])
