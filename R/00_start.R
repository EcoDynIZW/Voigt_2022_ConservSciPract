## ADD DOCUMENTATION TO YOUR PROJECT ----

## Add meta data about your project to DESCRIPTION
d6:::fill_desc(
  pkg_title = "TurbineCollisionDetection", 
  pkg_description = "Estimation of predictive power for acoustic monitoring on wind turbines",   
  author_first_name = "Cédric", 
  author_last_name = "Scherer",
  author_email = "scherer@izw-berlin.de",
  repo_url = NULL     
)

## Add and fill the readme
usethis::use_readme_md()

## Add license if needed
## See ?usethis::use_mit_license for more information
#usethis::use_mit_license( name = "Your Name" )

## ADD PACKAGE DEPENDENCIES ----

## Add one line by package you want to add as dependency
usethis::use_package("tidyverse")
