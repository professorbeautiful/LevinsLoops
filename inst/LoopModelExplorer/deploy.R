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
    appDir = paste0("inst/", app)
    tryCatch({
      require("shinyapps")
      if(file.exists(paste0(appDir, "/app.R")))
        deployApp(appDir = appDir, "app.R")
      else
        deployApp(appDir = appDir)
    },
    finally={
      cat("Try this:  shinyapps::showLogs(appDir = 'inst/", app, "')\n")
      }
    )
  }
}

.runDeployed = function(app="LoopModelExplorer/"){
  system(paste0("open https://trials.shinyapps.io/", app))
  cat(paste0("Try this:  shinyapps::showLogs(appPath = ',
             'inst/", app, "')\n"))
}

