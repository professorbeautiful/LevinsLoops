getLoopValue = function(loop, M) {
  print(loop)
  prod(
    sapply(1:(length(loop)-1), function(ind) M[loop[ind], loop[ind+1]])
  )
}

feedback_K = function(M) {
  loops = LoopAnalyst::enumerate.loops(M)
  nLoops = length(loops)
  choppedLoops = lapply(loops, '[', -1)
  loopLengths = sapply(loops, length) - 1
  powerSet = expand.grid(rep(list(c(FALSE,TRUE)), length(loops))) [ -1, ]
  t.powerSet = as.data.frame(t(powerSet))  # each column represents a loop set.
  loopSetIsDisjoint = sapply(t.powerSet, function(set)
    sum(duplicated(unlist(choppedLoops[which(set)]))) == 0 )
  loopSetStrings = sapply(t.powerSet, function(set)
    paste(choppedLoops[which(set)], collapse=",") )
  nLoopsPerSet = colSums(t.powerSet)  ### this is "m".
  nVarsPerSet = sapply(t.powerSet, function(set) # this is "k".
    length(unique(Reduce(union, choppedLoops[which(set)]))) )
  completeByLoopSet =  (nVarsPerSet == nrow(M))
  lengthsByLoopSet = unlist(apply(powerSet, 1, function(row) {
    unique(
      Reduce(sum, loopLengths[which(row)]))
  }))
  loopValues = sapply(loops, getLoopValue, M=M)
  setValues = sapply(t.powerSet, function(set) prod(loopValues[which(set)]))  ### Lmk.
  print(allData <- data.frame(loopProdStrings, nL=nLoopsPerSet, nV=nVarsPerSet, Lmk=setValues,
                   complete=completeByLoopSet, disj=loopSetIsDisjoint
             ))
  ## Lmk = L(m, k) is a product of m disjunct loops totaling k elements.
  Fk = sapply(1:nrow(M), function(k)
    with(allData[allData$nV==k & allData$disj, ],
         sum((-1)^(nL+1) * Lmk)
    )
  )
  return(Fk)
}
if(interactive()) feedback_K(cm.levins)
