library(stringr)
library(xtable)

# Run all files in the specific directory
sourceDirectory <- function(dirPath, Message = NULL){
  # Get list of files and functions (which MUST have the same names)
  pFiles <- paste(dirPath,list.files(dirPath),sep="")
  pFunctions <- str_match(pFiles,"/([^/]*)\\.R")[,2]
  # Source all files
  for(pNumber in 1:length(pFiles)){
    source(pFiles[pNumber],local = F)
  }
  if(is.null(Message)){
    cat(paste(Message,"\n"))
    # Print the list of all loaded functions
    print(pFunctions)
  }
  return(pFunctions)
}

# Make report directory and return the path
makeReportDirectory <- function(reportName = NULL){
  if(is.null(reportName)){
    fileName <- basename(sys.frame(1)$ofile)
  } else {
    fileName <- reportName
  }
  docDirName <- str_match(fileName,"(.*)\\.R")[,2]
  docPath <- paste("../Reports/",docDirName,sep="")
  dir.create(docPath, showWarnings = FALSE)
  return(docPath)
}

# generate Latex table from R table
generateLatexTable <- function(data, label = "", caption = "", digitsNum=2, align = ""){
  if(align == ""){
    align <- paste("|",paste(rep("c|",ncol(data)+1),sep="",collapse=""),sep="")
  }
  if(label == ""){
    varName <- deparse(substitute(data))
    tabLabel <- paste(varName,sep="")
  } else {
    tabLabel <- label
  }
  tabLabelRepl <- paste('\\\\label{',tabLabel,'}\n\\\\end{table}',sep="")
  latexTable <- xtable(tabl ,caption=caption,align=align,digits=digitsNum)
  resTable <- print(latexTable,print.results=FALSE)
  resTable <- str_replace_all(resTable, "\\\\end\\{table\\}",tabLabelRepl)
  return(resTable)
}

# data1 <- data.frame( methods = c("First", "Second", "Third"),
#                         error = c("0.5", "0.6", "0.7")
#   )

#cat(generateLatexTable(data1))

# Save R  table as latex table to file 
saveReportTable <- function(reportDir, data, filename="", label = "", caption = "", digitsNum=2, align = ""){
  if(label == ""){
    label <- deparse(substitute(data))
  }
  if(filename ==""){
    fname <- paste(reportDir,"/",label,".tex",sep="")
  } else {
    fname <-filename
  }
  resTab <- generateLatexTable(data,label,caption,digitsNum,align)
  fileConn<-file(fname,encoding="UTF-8")
  writeLines(resTab,fileConn)
  close(fileConn)
  return(fname)
}

#saveReportTable(reportDir,data1)
