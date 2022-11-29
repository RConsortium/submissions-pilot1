source("renv/activate.R")
if (Sys.getenv("GITHUB_ACTIONS") == "") {
  envsetup::rprofile(config::get("./_envsetup.yml"), config = "prod")
  source("inst/startup.R")
}
