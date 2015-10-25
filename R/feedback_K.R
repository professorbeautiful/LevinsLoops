feedback_K = function(M) {
  loops = LoopAnalyst::enumerate.loops(M)
  nLoops = length(loops)
  loopLengths = sapply(loops, length)
  disjointSets = list(loops)  ### start with M=1
  disjointNext = lapply(disjointSets,
                        function(set){


  })

  findDisjoint = function(rootSet, maxIndex) {
    repeat {
    }
  }
  possible = loops[[1]]
  for(L1 in 1:(nLoops-1))
    for(L2 in (L1+1):nLoops)
      if(
        length(intersect(loops[[L1]],loops[[L2]]))==0)
        disjointSets[[length(disjointSets)+1]] =
                         list(L1=loops[[L1]], L2=loops[[L2]],
                              loopLengths[L1], loopLengths[L2])
    disjointSets
}
if(interactive()) feedback_K(cm.levins)
