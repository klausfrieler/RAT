#' RAT feedback (no score)
#'
#' Here the participant is given no feedback at the end of the test.
#' @param dict The psychTestR dictionary used for internationalisation.
#' @export
#' @examples
#' \dontrun{
#' RAT_demo(feedback = RAT_feedback_no_score())}
RAT_feedback_with_score <- function(dict = RAT::RAT_dict) {
  psychTestR::new_timeline(
    c(
      psychTestR::reactive_page(function(state, ...) {
        results <- psychTestR::get_results(state = state, complete = TRUE, add_session_info = FALSE)
        results <- attr(as.list(results)$RAT$ability, "metadata")$results
        #print(results)
        sum_score <- sum(results$score)
        num_question <- nrow(results)
        #printf("Sum scores: %d, total items: %d", sum_score, num_question)
        text_finish <- psychTestR::i18n("COMPLETED",
                                        html = TRUE,
                                        sub = list(num_question = num_question, num_correct = sum_score))
        psychTestR::page(
          ui = shiny::div(
            shiny::p(text_finish),
            shiny::p(psychTestR::trigger_button("next", psychTestR::i18n("CONTINUE")))
          )
        )
      }
      )),
    dict = dict
  )

}
