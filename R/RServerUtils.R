
server.export <- function(x,
                          filename=NULL,
                          title=NULL,
                          format="CSV") 
{
  .DF <- x
  .FN <- filename
  .TT <- title
  .FM <- format
  
  
  # If filename is NULL -> ERROR
  # ---------------------------------------------------------------------------
  if (is.null(.FN)) {
    stop("You MUST provide a filename (without extension)")
  }
  
  if (is.null(.TT)) {
    .TT <- sprintf("exports/%s.%s", .FN, .FM)
  }
  
  # We are testing if 'exports' directory exists
  # If no, we create it
  # ---------------------------------------------------------------------------
  if (!dir.exists("exports")) {
    dir.create("exports")
  }
  
  # We create the file and we store it in exports directory
  # ---------------------------------------------------------------------------
  .FNAME <- sprintf("exports/%s.%s", .FN, .FM)
  .URL <- sprintf("<a href=\"%s.%s\" class=\"RExport\">%s</a>", .FN, .FM, .TT)
  # ---------------------------------------------------------------------------
  if (.FM == "CSV") {
    write.table(.DF, .FNAME, na="", sep=";", fileEncoding = "UTF-8")
    cat(.URL)
  }
  if (.FM == "RDS") {
    saveRDS(.DF, file = .FNAME)
    cat(.URL)
  }
}
