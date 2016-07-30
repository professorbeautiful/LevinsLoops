.installFromGithub = function(project)
  devtools::install_github(paste0("professorbeautiful/",
                                  project), build_vignettes=TRUE)

.deploy = function(project="LevinsLoops",
                   app="LoopModelExplorer", reInstall=TRUE){
  ## TODO: first check that the html files are created committed and pushed.
  if(reInstall)
    .installFromGithub(project)
  apps = app
  for (app in apps) {
    if(substr(app, 1, 5) == "inst/")
      warning(".deploy: do not include 'inst' in app name.")
    cat("wd is " %&% getwd() %&% "\n")
    cat("wd changing to " %&% "inst/" %&% app %&% "\n")
    setwd("inst/" %&% app)
    tryCatch({
      require("shinyapps")
      deployApp()
    },
    finally={
      cat("shinyapps::showLogs(appDir = 'inst/" %&% app %&% "')\n")
      setwd("../..")}
    )
  }
}

.runDeployed = function(app="LoopModelExplorer/"){
  system("open https://trials.shinyapps.io/" %&% app)
  cat("shinyapps::showLogs(appDir = 'inst/" %&% app %&% "')\n")
}

