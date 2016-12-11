print_table<-function(mat){
  addtorow          <- list()
  addtorow$pos      <- list()
  addtorow$pos[[1]] <- c(0)
  addtorow$command  <- c(
    paste(
      "\\hline \n",
      "\\endhead \n",
      "\\hline \n",
      "\\multicolumn{3}{l}{\\footnotesize Continued on next page} \n",
      "\\endfoot \n",
      "\\endlastfoot \n",sep = ""
    )
  )
  cat(
    sprintf(
      "\\begin{center}\n\\captionof{table}{Wide ranges of continious peaks (width>%d)}\n\\scriptsize",50
    )
  )
  print(
    xtable(
      mat)
    ,size = "small",include.colnames = TRUE,
    tabular.environment = "longtable",
    floating = FALSE,include.rownames = TRUE,
    add.to.row = addtorow,
    hline.after =c(-1)
  )
  cat("\\end{center}\n ")
}
