## Render your scripts to html
## Render all .Rmd files placed in ./R
# remotes::install_github("EcoDynIZW/d6)
d6::render_all_reports()

## Build Dockerfile
#devtools::install_github("karthik/holepunch")
holepunch::write_dockerfile()
