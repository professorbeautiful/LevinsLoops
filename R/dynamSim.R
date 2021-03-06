dynamSim = function(M,
                    initial, values,
                    constants, death=-100,
                    Tmax=15, timestep = 0.01,
                    plot=TRUE, print=FALSE,
                    findEq=TRUE,
                    returnLast=FALSE,
                    noNeg=TRUE,
                    useOdeSolver=FALSE,
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

  ######  using desolve::ode
  if(useOdeSolver)
    ode_output = ode(y=initial, times = seq(1,Tmax, by = timestep),
                     func=function(t, y, parms,...){
                       values = parms$values
                       list( c(constants +
                                 y %*% t(values)) )
                     },
                     parms = list(values=values),
                     method = "rk4"
    )

  ##### RESUMING  #####

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
  # Prevent overlap.
  offset = ((1:nSpecies)) * diff(range(trajectory))/200
  #trajectory = trajectory + outer(rep(1,ntimes), offset)
  if(plot) {
    timeline = seq(0,Tmax, by=timestep)[-1]
    plot(timeline, trajectory[ , 1], pch="",
         ylim = range(c(trajectory)),
         ylab="trajectories",
         main="")
    for(species in 1:nSpecies)
      lines(timeline, trajectory[ , species] + offset[species], col=species)
    abline(h=initial+offset, col=1:nSpecies, lty=2)
    legend(x = par()$usr[1], y = par()$usr[4], legend = rownames(M), yjust = 0, xpd = NA,
           text.col=1:nSpecies, horiz = TRUE)
    }
  if(findEq) {
    cat("predictedEq:\n")
    predictedEq = print(solve(values, -constants))
  }
  if(returnLast)  # Last
    returnVal = trajectory[length(timeline), ]
  else returnVal = trajectory
  if(attachAttributes) {
    attr(returnVal, "predictedEq") = predictedEq
    attr(returnVal, "effectMatrix") = LoopAnalyst::make.cem(M)
    if(useOdeSolver)
      attr(returnVal, "ode_output") = ode_output
    ### too much time!
    #`attr<-`(returnVal, "effectMatrix", 1:4) # syntax does not work.
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
#  print(revRows(dynamSim())[1,])
  dynamSim_output = dynamSim(initial=c(100,100,100,100), attachAttributes = TRUE)
