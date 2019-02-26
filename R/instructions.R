info_page <- function(id, style = "text-align:justify; margin-left:20%;margin-right:20%") {
  #messagef("Info page called with id %s and text %s", id, psychTestR::i18n(id, html = FALSE))
  psychTestR::one_button_page(shiny::div(psychTestR::i18n(id, html = TRUE), style = style),
                              button_text = psychTestR::i18n("CONTINUE"))
}

instructions <- function(audio_dir, img_dir) {
  c(
    psychTestR::code_block(function(state, ...) {
      psychTestR::set_local("do_intro", TRUE, state)
    }),
    show_sample_page(audio_dir, img_dir),
    info_page("INSTRUCTIONS"),
    #psychTestR::one_button_page(shiny::div(psychTestR::i18n("INSTRUCTIONS", html = TRUE),
    #                                       style = "text-align:justify; margin-left:20%;margin-right:20%"),
    #                            button_text = psychTestR::i18n("CONTINUE")),
    psychTestR::loop_while(
      test = function(state, ...) psychTestR::get_local("do_intro", state),
      logic = c(
        practice(audio_dir, img_dir)
        #ask_repeat()
      )),
    psychTestR::one_button_page(psychTestR::i18n("MAIN_INTRO"),
                                button_text = psychTestR::i18n("CONTINUE"))
  )
}

show_sample_page <- function(audio_dir, img_dir){
  sample_pattern <- "10111000"
  audio_url <- file.path(audio_dir, sprintf("%s.mp3", sample_pattern))
  img_src  <- file.path(img_dir, sprintf("%s.png", sample_pattern))
  audio <- get_audio_element(url = audio_url, autoplay = F)
  #messagef("Intro text %s", psychTestR::i18n("SAMPLE1a"))
  #messagef("Continue %s", psychTestR::i18n("CONTINUE"))
  body <- shiny::div(
    shiny::div(psychTestR::i18n("SAMPLE1a"),
             style = "text-align: justify; margin-left:20%; margin-right:20%"),
    shiny::p(audio),
    shiny::p(
      shiny::img(src = img_src, style = "width:600px")
    )
  )
  psychTestR::one_button_page(
    body = body,
    button_text = psychTestR::i18n("CONTINUE")
  )

}
