makeCMplot = function(CM) {
  dotFileCM = 'M.graphcm.dot'
  graph.cm(CM, file=dotFileCM)
  ### Replace the "odot" circle for negative link by "tee" or "odiamond" or "invempty"
  changeArrows = F
  if (changeArrows) {
    system("sed s/odot/invempty/ > M.graphcm.fixed.dot < M.graphcm.dot")
    dotFileCM = 'M.graphcm.fixed.dot'
  }
  outfile = paste0(dotFileCM, ".svg")
  dot(DOT = paste(collapse=" ",
                  readLines(dotFileCM)),
      file=outfile)
  return(outfile)
}
