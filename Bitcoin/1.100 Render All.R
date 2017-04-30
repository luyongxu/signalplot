render_html <- function(filename) { 
  rmarkdown::render(input = paste0("./Bitcoin/", filename, ".R"), 
                    output_file = paste0("./Bitcoin/Output/", filename, ".html"), 
                    knit_root_dir = ".")
}
render_html("1.001 Load Packages")
render_html("1.002 Load Data")
render_html("1.003 Engineer Features")
render_html("1.004 Plot Data")
