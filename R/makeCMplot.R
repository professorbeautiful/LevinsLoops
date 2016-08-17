makeCMplot = function(CM, return='auto',   changeArrows = F, size) {
  dotFileCM = 'M.graphcm.dot'
  graph.cm(CM, file=dotFileCM)
  ### Replace the "odot" circle for negative link by "tee" or "odiamond" or "invempty"
  if (changeArrows) {
    system("sed s/odot/invempty/ > M.graphcm.fixed.dot < M.graphcm.dot")
    system("cp M.graphcm.fixed.dot M.graphcm.dot")
  }
  if(!missing(size)) {
    system(paste0("sed s/18!/", size, "/g > M.graphcm.fixed.dot < M.graphcm.dot"))
    system("cp M.graphcm.fixed.dot M.graphcm.dot")
  }
  dotFileCM = 'M.graphcm.dot'
  outfile = paste0(dotFileCM, ".svg")
  DOT::dot(DOT = paste(collapse=" ",
                  readLines(dotFileCM)),
      file=outfile, return=return)
  return(outfile)
}
