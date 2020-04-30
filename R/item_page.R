g_default_width <- 75 * 4
g_default_height <- g_default_width/4
g_max_width <- 500

trigger_img_button <- function (inputId, img_src, width, height, margin = height/10){
  inputId <- htmltools::htmlEscape(inputId, attribute = TRUE)
  style <- sprintf("width: %dpx; height: %dpx; margin: %dpx; background: url('%s'); background-size: %dpx %dpx; background-position: center center;",
                   round(width), round(height), round(margin),
                   img_src, round(width), round(height))
  #printf("style = %s", style)
  shiny::actionButton(inputId = inputId,
                      label = "",
                      style = style,
                      icon = NULL,
                      onclick = "trigger_button(this.id);")
}

media_js <- list(
  media_not_played = "var media_played = false;",
  media_played = "media_played = true;",
  play_media = "document.getElementById('media').play();",
  show_media   = paste0("if (!media_played) ",
                        "{document.getElementById('media')",
                        ".style.visibility='inherit'};"),
  hide_media   = paste0("if (media_played) ",
                          "{document.getElementById('media')",
                          ".style.visibility='hidden'};"),
  show_media_btn = paste0("if (!media_played) ",
                          "{document.getElementById('btn_play_media')",
                          ".style.visibility='inherit'};"),
  hide_media_btn = paste0("document.getElementById('btn_play_media')",
                          ".style.visibility='hidden';"),
  show_responses = "document.getElementById('response_ui').style.visibility = 'inherit';"
)

#media_mobile_play_button <- shiny::tags$button(
#  shiny::tags$strong(psychTestR::i18n("CLICK_HERE_TO_PLAY")),
#  id = "btn_play_media",
#  style = "visibility: visible;height: 50px",
#  onclick = media_js$play_media
#)

media_mobile_play_button <- shiny::tags$p(
  shiny::tags$button(shiny::tags$span("\u25B6"),
                     type = "button",
                     id = "btn_play_media",
                     style = "visibility: hidden",
                     onclick = media_js$play_media)
)
get_audio_ui <- function(url,
                         type = tools::file_ext(url),
                         autoplay = TRUE,
                         width = 0,
                         wait = TRUE,
                         loop = FALSE) {
  #print(url)
  stopifnot(purrr::is_scalar_character(url),
            purrr::is_scalar_character(type),
            purrr::is_scalar_logical(wait),
            purrr::is_scalar_logical(loop))
  src    <- shiny::tags$source(src = url, type = paste0("audio/", type))
  script <- shiny::tags$script(shiny::HTML(media_js$media_not_played))
  audio  <- shiny::tags$audio(
    script,
    src,
    id = "media",
    preload = "auto",
    autoplay = if(autoplay) "autoplay",
    width = width,
    loop = if (loop) "loop",
    oncanplaythrough = media_js$show_media_btn,
    onplay = paste0(media_js$media_played, media_js$hide_media_btn),
    #onended = if (wait) paste0(media_js$show_responses, media_js$hide_media) else "null",
    onended = if (wait) media_js$show_responses else "null"
  )
  shiny::tags$div(audio, media_mobile_play_button)
}

get_audio_element <- function(url,
                              type = tools::file_ext(url),
                              wait = F,
                              autoplay = FALSE,
                              width = 200,
                              height = 50,
                              id = "media") {
  #print(url)
  stopifnot(purrr::is_scalar_character(url),
            purrr::is_scalar_character(type)
            )
  src    <- shiny::tags$source(src = url, type = paste0("audio/", type))
  script <- shiny::tags$script(shiny::HTML(media_js$media_not_played))
  audio  <- shiny::tags$audio(
    src,
    script,
    id = id,
    preload = "auto",
    controls = "controls",
    controlslist = "nodownload noremoteplayback",
    autoplay = if(autoplay) "autoplay",
    width = width,
    height = height,
    onplay = paste0(media_js$media_played, media_js$hide_media),
    onended = if (wait) paste0(media_js$show_responses, media_js$hide_media) else "null"
  )
  audio
}

audio_NAFC_page_with_img <- function(label,
                                     prompt,
                                     choices,
                                     audio_url,
                                     save_answer = TRUE,
                                     get_answer = NULL,
                                     hide_response_ui = TRUE,
                                     response_ui_id = "response_ui",
                                     on_complete = NULL,
                                     admin_ui = NULL) {
  stopifnot(purrr::is_scalar_character(label))
  #audio_ui <- get_audio_ui(audio_url, wait = T, loop = F)
  audio_ui <- get_audio_ui(audio_url, wait = T, loop = F)
  #audio_ui <- get_audio_element(audio_url, autoplay = T, wait = T, width = 50)
  style <- NULL
  if(hide_response_ui) style <- "visibility:hidden"
  ui <- shiny::div(
    tagify(prompt),
    audio_ui,
    shiny::div(choices, style = style, id = response_ui_id)
    )
  if(is.null(get_answer)){
    get_answer <- function(input, ...) as.numeric(gsub("answer", "", input$last_btn_pressed))
  }
  validate <- function(answer, ...) !is.null(answer)
  #printf("[audio_NAFC_page_with_img] left")
  psychTestR::page(ui = ui, label = label,  get_answer = get_answer, save_answer = save_answer,
       validate = validate, on_complete = on_complete, final = FALSE,
       admin_ui = admin_ui)
}
get_answer_button <- function(bin_code,
                              width = g_default_width, height = g_default_height, index,
                              img_dir = "https://media.gold-msi.org/test_materials/RAT2/img_inv"){

  img_src <- file.path(img_dir, sprintf("%s.png", bin_code))
  #printf("get_answer_button img_src: %s", img_src)
  #printf("Get answer button width %.0f -> %.0f", width, min(width, g_max_width) )
  #rescale_factor <- g_max_width/width
  #width <- min(width, g_max_width)
  #height <-
  img_button <- trigger_img_button(inputId = sprintf("answer%d", index),
                                   width = width,
                                   height = height,
                                   margin = height/10,
                                   img_src = img_src)
  img_button
}
get_answer_block<-function(bin_codes,
                           width = g_default_width, height = g_default_height, ncols = 2,
                           img_dir = "https://media.gold-msi.org/test_materials/RAT2/img_inv",
                           ...){
  n <- length(bin_codes)
  rows <- list()
  for(i in seq_len(n)){
    width_factor <- nchar(bin_codes[i])/8
    rescale <- 1
    if(nchar(bin_codes[i]) == 16){
      rescale <- .75
    }
    button <- get_answer_button(bin_codes[i],
                                width = width * width_factor * rescale,
                                height = height * rescale,
                                index = i,
                                img_dir = img_dir)
      rows[[i]] <- button
  }

  ret <- list()
  nrows <- floor(n/ncols)
  for(i in seq_len(nrows)){
    ret[[i]] <- shiny::div(rows[(i-1) * ncols + (1:ncols)])
  }
  #shiny::div(ret, ...)
  ret
}

RAT_item <- function(label = "",
                     pattern,
                     lures,
                     answer,
                     prompt = "",
                     img_dir = "",
                     audio_dir = "",
                     save_answer = TRUE,
                     get_answer = NULL,
                     on_complete = NULL,
                     instruction_page = FALSE,
                     block_size = 4){

  page_prompt <- shiny::div(prompt)
  #printf("RAT item_called for pattern %s", pattern)

  if(!instruction_page){
    bin_codes <- rep(pattern, block_size)
    bin_codes[setdiff(1:block_size, answer)] <- lures
    choices <- get_answer_block(bin_codes, img_dir = img_dir)
    audio_url <- file.path(audio_dir, sprintf("%s.mp3", pattern))
    audio_NAFC_page_with_img(label = label,
                             prompt = page_prompt,
                             audio_url = audio_url,
                             choices = choices,
                             save_answer = save_answer,
                             get_answer = get_answer,
                             on_complete = on_complete)
  }
  else{
    #print("Instruction page hand")
    psychTestR::one_button_page(page_prompt, button_text = "add stuff")
  }
}

