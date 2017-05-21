getLoopValue = function(loop, M) {
  print(loop)
  prod(
    sapply(1:(length(loop)-1), function(ind) M[loop[ind+1], loop[ind]])
  )
}

feedback_K = function(M) {
  loops = LoopAnalyst::enumerate.loops(M)
  loopNames = sapply(loops, paste, collapse="-")
  nLoops = length(loops)
  choppedLoops = lapply(loops, '[', -1)
  loopLengths = sapply(loops, length) - 1
  powerSet = expand.grid(rep(list(c(FALSE,TRUE)), length(loops))) [ -1, ]
  names(powerSet) = loopNames
  t.powerSet = as.data.frame(t(powerSet))  # each column represents a loop set.
  loopSetIsDisjoint = sapply(t.powerSet, function(set)
    sum(duplicated(unlist(choppedLoops[which(set)]))) == 0 )
  disjointSets = t.powerSet [, loopSetIsDisjoint]
  loopSetStrings = sapply(disjointSets, function(set)
    paste(choppedLoops[which(set)], collapse=",") )
  nLoopsPerSet = colSums(disjointSets)  ### this is "m".
  nVarsPerSet = sapply(disjointSets, function(set) # this is "k".
    length(unique(Reduce(union, choppedLoops[which(set)]))) )
  completeByLoopSet =  (nVarsPerSet == nrow(M))
  lengthsByLoopSet = unlist(apply(disjointSets, 2, function(col) {
    unique(
      Reduce(sum, loopLengths[which(col)]))
  }))
  loopValues = sapply(loops, getLoopValue, M=M)
  setValues = sapply(disjointSets, function(set) prod(loopValues[which(set)]))  ### Lmk.
  print(allData <- data.frame(loopSetStrings, `nL.m.`=nLoopsPerSet, `nV.k.`=nVarsPerSet, Lmk=setValues,
                   complete=completeByLoopSet
             ))
  ## Lmk = L(m, k) is a product of m disjunct loops totaling k elements.
  Fk = sapply(1:nrow(M), function(k) {
    kData = allData[allData$nV.k.==k, ]
    print(kData)
    return( sum( (-1) * (-1)^kData$`nL.m.` * kData$Lmk ) )
  }
  )
  return(Fk)
}
if(interactive()) feedback_K(cm.levins)

# stabilityCriteria = function(M) {
#   Fk =
# }
