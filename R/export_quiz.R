#' Export quiz as TeX
#'
#' @param quiz a data.frame created by import_quiz
#' @param output_file the location to save the TeX document
#' @param class_name name of class to be used on quiz
#' @param quiz_name name of quiz to be used on quiz
#' @param n_ver number of versions to create when TeX is compiled
#' @return outputs a TeX file to output_file location
#' @export
export_quiz <- function(quiz, output_file, class_name, quiz_name, n_ver){
  sink(output_file)
  cat("\\documentclass[12pt]{examdesign}\n\n")
  cat("\\class{", class_name, "}\n", sep = "")
  cat("\\examname{", quiz_name, "}\n", sep = "")
  cat("\\NumberOfVersions{", n_ver, "}\n\n", sep = "")
  cat("\\begin{document}\n\n")
  cat("\\begin{multiplechoice}\n")
  for(i in 1:nrow(quiz)){
    cat("\t% Question", i, "\n", sep = "")
    cat("\t\\begin{question}\n")
    cat("\t\t", paste(quiz$question[i]), "\n", sep = "")
    for(j in 1:(ncol(quiz)-2)){
      if(!is.na(quiz[i,j])){
        if(quiz$correct_answer[i] == j){
          cat("\t\t\\choice[!]{", paste(quiz[i,j]), "}\n", sep = "")
        } else{
          cat("\t\t\\choice{", paste(quiz[i,j]), "}\n", sep ="")
        }
      }
    }
    cat("\t\\end{question}\n")
  }
  cat("\\end{multiplechoice}\n")
  cat("\\end{document}")
}
