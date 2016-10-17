library(XML)

data <- xmlParse("quiz1.xml")

data <- xmlToList(data)

## questions
# data$assessment$section$item$presentation$material$mattext$text
## answer
# data$assessment$section$item$presentation$response_lid$render_choice$response_label$material$mattext$text

questions <- list()
all_answers <- list()
for (i in 1:length(data$assessment$section)) {
  item <- data$assessment$section[[i]]
  questions[i] <- item$presentation$material$mattext$text
  ## answers <- list()
  ## for (j in 1:length(item$presentation$response_lid$render_choice)) {
  ##   render_choice <- item$presentation$response_lid[[j]]
  ##   answers[j] <- render_choice$response_label$material$mattext$text
  ##   all_answers <- answers
  ## }
}

unlist(questions)
