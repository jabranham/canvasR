library(XML)

## For now, let's assume only multiple choice questions
## quiz1.xml should be an exported quiz from Canvas.
data <- xmlParse("quiz1.xml")

data <- xmlToList(data)

## questions
# data$assessment$section$item$presentation$material$mattext$text
## answer
# data$assessment$section$item$presentation$response_lid$render_choice$response_label$material$mattext$text

questions <- list()
all_answers <- list()
for (i in 1:length(data$assessment$section)) {
  ## For each question, get the question text:
  item <- data$assessment$section[[i]]
  questions[i] <- item$presentation$material$mattext$text
  answers <- list()
  for (j in 1:length(item$presentation$response_lid$render_choice)) {
    ## For all answers, get the answer text
    response_label <- item$presentation$response_lid$render_choice[[j]]
    answers[j] <- response_label$material$mattext$text
    if (j == length(item$presentation$response_lid$render_choice)) {
      ## Put the answers in a list
      all_answers[[i]] <- unlist(answers)
    }
  }
}

output <- data.frame(matrix(unlist(all_answers),
                           nrow = length(all_answers), byrow = TRUE))
output$question <- unlist(questions)
