quiz <- XML::xmlParse("quiz.xml")
quiz <- XML::xmlToList(quiz)
devtools::use_data(quiz)
