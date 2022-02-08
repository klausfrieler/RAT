#source("R/RAT.R")
#options(shiny.error = browser)
#debug_locally <- !grepl("shiny-server", getwd())
#RAT_study_id <- 25


#' Standalone RAT
#'
#' This function launches a standalone testing session for the RAT
#' This can be used for data collection, either in the laboratory or online.
#' @param title (Scalar character) Title to display during testing.
#' @param num_items (Scalar integer) Number of items to be adminstered.
#' @param with_feedback (Scalar boolean) Indicates if performance feedback will be given at the end of the test. Defaults to  FALSE
#' @param take_training (Boolean scalar) Defines whether instructions and training are included.
#' Defaults to TRUE.
#' @param admin_password (Scalar character) Password for accessing the admin panel.
#' @param researcher_email (Scalar character)
#' If not \code{NULL}, this researcher's email address is displayed
#' at the bottom of the screen so that online participants can ask for help.
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include English (\code{"en"}),
#' German (informal: \code{"de"} and formal \code{"de_f"}), and Italian (\code{"it"})
#' The first language is selected by default
#' @param dict The psychTestR dictionary used for internationalisation.
#' @param validate_id (Character scalar or closure) Function for validating IDs or string "auto" for default validation
#' which means ID should consist only of  alphanumeric characters.
#' @param ... Further arguments to be passed to \code{\link{RAT}()}.
#' @export
RAT_standalone  <- function(title = NULL,
                           num_items = 16L,
                           with_id = FALSE,
                           with_feedback = FALSE,
                           take_training = TRUE,
                           with_welcome = TRUE,
                           admin_password = "conifer",
                           researcher_email = "longgold@gold.uc.ak",
                           languages = c("en", "de", "de_f", "it"),
                           dict = RAT::RAT_dict,
                           validate_id = "auto",
                           ...) {
  feedback <- NULL
  if(with_feedback) {
    #feedback <- RAT_feedback_with_score()
    feedback <- RAT_feedback_with_graph()
  }
  elts <- psychTestR::join(
    if(with_id) psychTestR::new_timeline(
      psychTestR::get_p_id(prompt = psychTestR::i18n("ENTER_ID"),
                           button_text = psychTestR::i18n("CONTINUE"),
                           validate = validate_id),
      dict = dict
    ),
    #register_participant(),
    RAT(num_items = num_items,
        take_training = take_training,
        with_welcome =  with_welcome,
        feedback = feedback,
        ...),
    #psychTestRCAT::cat.feedback.graph("RAT"),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    #upload_results(F),
    psychTestR::new_timeline(
      psychTestR::final_page(shiny::p(
        psychTestR::i18n("RESULTS_SAVED"),
        psychTestR::i18n("CLOSE_BROWSER"))
      ), dict = dict)
  )
  languages <- tolower(languages)
  if(is.null(title)){
    title <- map_chr(languages, ~{RAT::RAT_dict$translate("TESTNAME", .x)}) %>% set_names(languages)
  }
  psychTestR::make_test(
    elts,
    opt = psychTestR::test_options(title = title,
                                   admin_password = admin_password,
                                   researcher_email = researcher_email,
                                   demo = FALSE,
                                   languages = tolower(languages)))
}
