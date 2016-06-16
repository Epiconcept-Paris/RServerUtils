
server.export <- function(x,
                          filename=NULL) {
  if (!dir.exists("exports")) {
    dir.create("exports")
  }
  
}
data(mtcars)
server.export(mtcars)