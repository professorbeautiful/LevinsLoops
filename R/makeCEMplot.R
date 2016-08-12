makeCEMplot = function(CM) {
  CEM = make.cem(CM)
  CEM = t(CEM) ### Correction
  dotFileCEM = 'M.graphcem.dot'
  graph.cem(CEM, file=dotFileCEM)
  changeArrows = F
  if (changeArrows) {
    system("sed s/odot/invempty/ > M.graphcem.fixed.dot < M.graphcem.dot")
    dotFileCEM = 'M.graphcem.fixed.dot'
  }
  outfile = paste0(dotFileCEM, ".svg")
  dot(DOT = paste(collapse=" ",
                  readLines(dotFileCEM)),
      file=outfile)
  return(outfile)
}
