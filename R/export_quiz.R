#' Export quiz as TeX
#'
#' @param quiz a data.frame created by import_quiz
#' @param output_file the location to save the TeX document
#' @param class_name name of class to be used on quiz
#' @param quiz_name name of quiz to be used on quiz
#' @param n_ver number of versions to create when TeX is compiled
#' @param shuffle Logical indicating whether answer choices should be randomized
#' @return outputs a TeX file to output_file location
#' @export
export_quiz <- function(quiz, output_file, class_name = "class name", quiz_name = "quiz name", n_ver = 1, shuffle = F){
  sink(output_file)
  cat("\\documentclass[12pt]{examdesign}\n\n")
  cat("\\class{", class_name, "}\n", sep = "")
  cat("\\examname{", quiz_name, "}\n", sep = "")
  cat("\\NumberOfVersions{", n_ver, "}\n\n", sep = "")
  cat("\\begin{document}\n\n")
  cat("\\begin{multiplechoice}\n")
  if(shuffle == FALSE){
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
  } else{
    for(i in 1:nrow(quiz)){
      answers <- which(!is.na(quiz[i, 1:(ncol(quiz)-2)]))
      l_answers <- length(answers)
      correct_shuffle <- sample(answers, 1)
      correct_orig <- as.numeric(quiz$correct_answer[i])
      correct_answer_vec <- quiz[i, correct_orig]
      incorrect_answer_vec <- quiz[i, answers]
      incorrect_answer_vec <- incorrect_answer_vec[,-c(correct_orig)]
      # need to convert to df if only 1 incorrect answer to avoid problems below
      if(!is.data.frame(incorrect_answer_vec)){
        incorrect_answer_vec <- data.frame(x = as.character(incorrect_answer_vec))
      }
      pre_spaces <- correct_shuffle-1
      post_spaces <- l_answers-correct_shuffle

      cat("\t% Question", i, "\n", sep = "")
      cat("\t\\begin{question}\n")
      cat("\t\t", paste(quiz$question[i]), "\n", sep = "")

      if(pre_spaces == 0){
        cat("\t\t\\choice[!]{", paste(as.character(correct_answer_vec)), "}\n", sep = "")
        for(j in 1:ncol(incorrect_answer_vec)){
          cat("\t\t\\choice{", paste(incorrect_answer_vec[1,j]), "}\n", sep ="")
        }
      } else if(post_spaces == 0) {
        for(j in 1:ncol(incorrect_answer_vec)){
          cat("\t\t\\choice{", paste(incorrect_answer_vec[1,j]), "}\n", sep ="")
        }
        cat("\t\t\\choice[!]{", paste(as.character(correct_answer_vec)), "}\n", sep = "")
      } else{
        for(j in 1:pre_spaces){
          cat("\t\t\\choice{", paste(incorrect_answer_vec[1,j]), "}\n", sep ="")
        }
        cat("\t\t\\choice[!]{", paste(as.character(correct_answer_vec)), "}\n", sep = "")
        for(j in pre_spaces+1:post_spaces){
          cat("\t\t\\choice{", paste(incorrect_answer_vec[1,j]), "}\n", sep ="")
        }
      }
      # answer choices go here
      cat("\t\\end{question}\n")
    }
  }

  cat("\\end{multiplechoice}\n")
  cat("\\end{document}")
  closeAllConnections()
}
