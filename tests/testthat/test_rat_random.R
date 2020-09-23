#Automated testing for RAT
library(psychTestR)
library(testthat)

dir <- system.file("RAT", package = "RAT", mustWork = TRUE)

number_items <- 14 #number of items #?

app <<- AppTester$new(dir)

# ID
app$expect_ui_text("Please enter your particpant ID. Continue") # SIC!
app$set_inputs(p_id = "abcde")
app$click_next()

# Training
app$expect_ui_text("Welcome to the Musical Rhythm Test In this test you will hear a number of short rhythms which you have to match to a corresponding image. The rhythms will be preceeded and followed by four metronome clicks each. First you'll hear a few examples and do some practice questions. Continue")
app$click_next()

app$expect_ui_text("Example 1 Each rhythm consists of claps and bass drum kicks. The claps are represented by light blue squares in the upper row and the bass drum by dark blue squares in the lower row. Listen to this short rhythm by clicking the play button.There will be 4 metronome clicks before and after the rhythm. Continue")
app$click_next()

app$expect_ui_text("The rhythms will all have four, eight or sixteen sounds and four clicks of a metronome before and after the actual rhythm. Your task is to listen to the rhythm and then click on the one image from among four options that corresponds to the rhythm that you just heard. Let's have some practice. Continue")
app$click_next()

# app$expect_ui_text("Practice Question 1 Here is a practice question with four elements. <U+25B6> Continue") # ???
app$click("answer3")

# app$expect_ui_text("Practice Question 2 Correct! Let's try a practice question with eight sounds. Click here to play")
app$click("answer1")

# app$expect_ui_text("Practice Question 3 Correct! Let's try a final practice question with sixteen sounds. Click here to play")
app$click("answer1")

app$expect_ui_text("Incorrect. Press 'Go back' to read the instructions and do the practice questions again, or press 'Continue' to continue to the main test. Go back Continue")
app$click("continue") # "go_back"

app$expect_ui_text("You are about to start the main test, where your results will be recorded. You will only be able to hear each rhythm once. You won't receive any feedback after individual questions. Good luck! Continue")
app$click_next() # "go_back"

# Main test
q <- 1 # number of question
for (i in sample(1:4, number_items, replace=TRUE)) {
  # app$expect_ui_text(paste("Question", q, "out of", number_items, "Which image matches the rhythm you just heard? Click on the correct one. If you don't know, give your best guess! <U+25B6>")) # ???
  app$click(paste0("answer", i))
  print(paste0("answer", i))
  q <- q + 1
}

app$expect_ui_text("You finished the Musical Rhythm Test. Continue")

# Results
results <- app$get_results() %>% as.list()
print(j)
RAT_SEM[j] <<- results[["RAT"]][["ability_sem"]]
RAT_ability[j] <<- results[["RAT"]][["ability"]]

print(paste("Standard error of measurement of RAT", RAT_SEM[j]))
app$stop()

