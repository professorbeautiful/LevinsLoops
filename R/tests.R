####  tests of the cem calculation
testNode = function(from, to, M=cm.levins, delta = 0.01,
                    returnIdentical=FALSE,
                    ...) {
  change =
    dynamSim(M=M, returnLast = TRUE, ...,
             values=modifyValues(M, from, to, delta)) -
    dynamSim(M=M, returnLast = TRUE, ...)
  prediction = LoopAnalyst::make.cem(M)[ to, ]
  if(returnIdentical) return(
    identical(sign(change), sign(prediction))
    )
  rbind(change, prediction)
}

if(interactive()) {
  data(cm.levins)
  out.cm(cm.levins)
  data(cem.levins)
  cm.levins
  cem.levins
  graph.cm(cm.levins, file="cm.levins.graphcm.dot")
  graph.cem(cem.levins, file="cem.levins.graphcem.dot")
  system("open cm.levins.graphcm.dot")
  system("open cem.levins.graphcem.dot")

  cm.simplest = matrix(-1)
  dimnames(cm.simplest) = list("P","P")
  dynamSim(M=cm.simplest, death = 0.1)

  enumerate.loops(cm.levins)
  feedback(cm.levins)
  enumerate.loops(cm.simplest)

  dynamSim(returnLast = TRUE)
  sign(round(testNode("R", "R", Tmax=10)))
  for(sp1 in rownames(cm.levins)) {
    cat(sp1, "\n")
    for(sp2 in rownames(cm.levins)) {
      result = testNode(sp1, sp2, Tmax=10, returnIdentical = TRUE)
      cat("    ", sp2, " ", result, "\n")
      if(result == FALSE)
        print(testNode(sp1, sp2, Tmax=10))
    }
  }

  dynamSim(M=cbind(c(-1+0.50,1), c(-1, 0.01)),
           Tmax=100, returnLast=T)
  dynamSim(M=cbind(c(-1+0.00,1), c(-1, 0.01)),
           Tmax=100, returnLast=T)
  testNode(1, 1, M=cbind(c(-1,1), c(-1, 0)), Tmax=100) ### Fig 4a
  #OK
  testNode(1, 2, M=cbind(c(-1,1), c(-1, 0)), Tmax=100, noNeg=F) ### Fig 4a
  #OK
  testNode(1, 1, M=cbind(c(-1,1), c(-1, +0.02)),
           delta=1e-5, Tmax=100) ### Fig 4b
  testNode(1, 2, M=cbind(c(-1,1), c(-1, +1)), Tmax=100, noNeg=F) ### Fig 4b

}  ### end interactive()
