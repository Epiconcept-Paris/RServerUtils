
server.export <- function(x,
                          filename=NULL,
                          title=NULL,
                          format="csv",
                          rownames=FALSE) 
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
  if (.FM == "csv") {
    write.table(.DF, .FNAME, na="", sep=";",
                fileEncoding = "UTF-8",
                row.names = rownames)
    cat(.URL)
  }
  if (.FM == "rds") {
    saveRDS(.DF, file = .FNAME)
    cat(.URL)
  }
  if (.FM == "xlsx") {
    write.xlsx(.DF, file = .FNAME, row.names = rownames, showNA = FALSE)
    cat(.URL)
  }
  
}

figure.export <- function(x,
                          filename=NULL,
                          title=NULL,
                          format="png",
                          plot.width=1120,
                          plot.height=630) {

  .FN <- filename
  .TT <- title
  .W <- plot.width
  .H <- plot.height

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
  
  URLFMT <- "<a href=\"%s.%s\" class=\"RExport\">%s: %s</a><br />"
  if (is.null(x)) {
    for (fmt in c(format)) {
      fname <- sprintf("exports/%s.%s", filename, fmt)
      if (fmt == "png") {
        dev.copy(png, fname, width=.W, height=.H); dev.off()
        .URL <- sprintf(URLFMT, .FN, fmt, fmt, .TT)
        cat(.URL)
      }
      if (fmt == "jpg") {
        dev.copy(jpeg, fname, width=.W, height=.H); dev.off()
        .URL <- sprintf(URLFMT, .FN, fmt, fmt, .TT)
        cat(.URL)
      }
      if (fmt == "pdf") {
        dev.copy(pdf, fname, width=.W, height=.H); dev.off()
        .URL <- sprintf(URLFMT, .FN, fmt, fmt, .TT)
        cat(.URL)
      }
      if (fmt == "svg") {
        dev.copy(svg, fname, width=.W, height=.H); dev.off()
        .URL <- sprintf(URLFMT, .FN, fmt, fmt, .TT)
        cat(.URL)
      }
    }
  }
  else {
    if (class(x)[1] == 'gg') {
      for (fmt in c(format)) {
        fname <- sprintf("exports/%s.%s", filename, fmt)
        ggsave(fname, plot = x, device = fmt)
        .URL <- sprintf(URLFMT, .FN, fmt, fmt, .TT)
        cat(.URL)
      }
    }
  }
}
