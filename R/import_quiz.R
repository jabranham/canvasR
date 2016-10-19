#' Import quizzes from xml objects
#'
#' @param file a character containing the location of a Canvas-exported quiz.xml file
#' @param html_patterns a character vector of patterns to strip from question text
#' @param answer_fill what to fill answers with in the returned data.frame if it isn't perfectly rectangular
#' @return a data.frame containing questions and answers
#' @examples
#' quiz <- system.file("extdata", "quiz.xml", package = "canvasR")
#' import_quiz(quiz)
#' @export
import_quiz <- function(file,
                       html_patterns = c("<div>", "</div>", "<p>", "</p>"),
                       answer_fill = NA){
  data <- XML::xmlToList(XML::xmlParse(file))
  questions <- list()
  all_answers <- list()
  correct_answer_position <- list()
  for (i in 1:length(data$assessment$section)) {
    ## For each question, get the question text:
    item <- data$assessment$section[[i]]
    if (class(item) != "character"){
      if (item$itemmetadata$qtimetadata$qtimetadatafield$fieldentry %in% c("multiple_choice_question", "true_false_question")){
        questions[i] <- item$presentation$material$mattext$text
        answers <- list()
        answer_labels <- list()
        for (j in 1:length(item$presentation$response_lid$render_choice)) {
          ## For all answers, get the answer text
          response_label <- item$presentation$response_lid$render_choice[[j]]
          if (class(response_label$material$mattext) != "character"){
            answers[j] <- response_label$material$mattext$text
            answer_labels[j] <- as.numeric(response_label$.attrs)
          }
          if (j == length(item$presentation$response_lid$render_choice)) {
            ## Put the answers in a list
            all_answers[[i]] <- unlist(answers)
          }
        }
        # pull correct answer id with or without response feedback
        response_processing <- item$resprocessing
        correct_answer_position[i] <- NA
        correct_answer <- as.numeric(response_processing[[length(response_processing)]]$conditionvar$varequal$text)
        if (length(correct_answer) == 1){
          correct_answer_position[i] <- grep(correct_answer, answer_labels)
        }  else{
          countdown <- length(response_processing)
          while (length(correct_answer) == 0 & countdown > 1){
            if (names(response_processing[[countdown - 1]][2]) == "setvar"){
              correct_answer <- as.numeric(response_processing[[countdown - 1]]$conditionvar$varequal$text)
              correct_answer_position[i] <- grep(correct_answer, answer_labels)
            } else{
              countdown <- countdown - 1
            }
          }
        }
      }
      else{
        warning(paste0("Question ", i,
                       " is not a multiple choice or true/false question. It has been ommitted from the output."))
      }
    }
  }

  # in case not all questions have the same number of possible answers
  max_answers <- 0
  for (k in 1:length(all_answers)){
    if (length(all_answers[[k]]) > max_answers){
      max_answers <- length(all_answers[[k]])
    }
  }
  for (l in 1:length(all_answers)){
    if (length(all_answers[[l]] < max_answers)){
      all_answers[[l]] <- c(all_answers[[l]],
                           rep(answer_fill, max_answers - length(all_answers[[l]])))
    }
    if (is.null(all_answers[[l]])){
      all_answers[[l]] <- rep(answer_fill, max_answers)
    }
  }

  output <- data.frame(matrix(unlist(all_answers),
                             nrow = length(all_answers), byrow = TRUE))
  output$question <- unlist(questions)
  output$correct_answer <- unlist(correct_answer_position)

  # take care of html from questions or answers
  for (m in 1:length(html_patterns)){
    output <- as.data.frame(apply(output, c(1, 2),
                                 function(x) gsub(html_patterns[m], "", x)))
  }
  return(output)
}
