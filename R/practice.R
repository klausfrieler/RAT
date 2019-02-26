training_patterns <- c("0101",
                       "01011100",
                       "1010100110101000")
training_answers  <- c(3, 1, 4, -1)
training_lures    <- list(c("0100",
                            "0001",
                            "1101"),
                          c("01011110",
                            "01011101",
                            "00011100"),
                          c("1010100110101010",
                            "1010100110100000",
                            "1010100110101001"))

ask_repeat <- function(prompt) {
  psychTestR::NAFC_page(
    label = "ask_repeat",
    prompt = prompt,
    choices = c("go_back", "continue"),
    labels = lapply(c("GOBACK", "CONTINUE"), psychTestR::i18n),
    save_answer = FALSE,
    arrange_vertically = FALSE,
    on_complete = function(state, answer, ...) {
      psychTestR::set_local("do_intro", identical(answer, "go_back"), state)
    }
  )
}

make_practice_page <-  function(page_no, audio_dir, img_dir) {
  psychTestR::reactive_page(function(answer, ...) {
    #printf("[make_practice_page] Answer: %s, page_no: %d, correct: %d", answer, page_no, training_answers[page_no])
    correct <- "INCORRECT"
    if (page_no > 1 &&  answer == training_answers[page_no-1]) correct <- "CORRECT"
    feedback <- psychTestR::i18n(correct)
    get_practice_page(page_no, feedback, audio_dir, img_dir)
  })
}



get_practice_page <- function(page_no, feedback, audio_dir, img_dir){
  #printf("get_practice_page] called")
  key <- sprintf("PRACTICE%d", page_no)

  if(page_no == 4) key <- "TRANSITION"
  prompt <- psychTestR::i18n(key, html = T, sub = list(feedback = feedback))
  #printf("[get_practice_page] page_no; %d, key: %s, feedback: %s", page_no, key, feedback)

  if(page_no == 4){
    #page <- psychTestR::one_button_page(body = prompt,
    #                                    button_text = psychTestR::i18n("CONTINUE"))
    page <- ask_repeat(prompt)
  }
  else{
    page <- RAT_item(label = sprintf("training%s", page_no),
                     pattern = training_patterns[page_no],
                     lures = training_lures[[page_no]],
                     answer = training_answers[page_no],
                     prompt = prompt,
                     img_dir = img_dir,
                     audio_dir = audio_dir,
                     save_answer = FALSE,
                     instruction_page = FALSE)
  }
  page
}

practice <- function(audio_dir, img_dir) {
  lapply(1:4, make_practice_page, audio_dir, img_dir) %>% unlist()
}
