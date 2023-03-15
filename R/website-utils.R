#' Title
#'
#' @param name
#' @param label
#' @param value
#' @param min
#' @param max
#' @param step
#' @param scale_name
#' @param width
#'
#' @return
#' @export
#'
#' @examples
input_slider <- function(name, label, value, min, max, step = 1, width = "50%", scale_name = "scale") {
  htmltools::HTML(glue::glue('
  <label for="input-{name}" style="font-family: KaTeX_Main; font-size: 1em; font-style: italic; height: 1em;">{label}</label>
  <span id="value-{name}" class="math" style="display: inline-block; width: 1em;">{value}</span>
  <input data-prevent-swipe type="range" id="input-{name}" name="{scale_name}" value="{value}" min="{min}" max="{max}" step="{step}" style="border: none; height: 1em; font-family: KaTeX_Main; font-size: 1em; margin-left: 0.5em; width: {width}; vertical-align: unset;">

                             '))
}


#' Title
#'
#' @param name
#' @param label
#' @param value
#'
#' @return
#' @export
#'
#' @examples
input_text <- function(name, label, value,
                       label_class = "", label_style = "",
                       value_class = "", value_style = "") {
  htmltools::HTML(glue::glue('

  <label for="input-{name}" class = "{label_class}" style="{label_style}">{label}
  <input type="text" id="input-{name}" class="{value_class}" name="{name}" value="{value}" style="{value_style}">
  </label>
                             '))

}











pdf_download_icon <- function() {
  htmltools::HTML(
    '
    <a class="invertable" href="syllabus.pdf"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 384 600" style="height:0.75em" aria-labelledby="downloadIconTitle" role="img"><title id="downloadIconTitle">Download PDF version of syllabus</title><!--! Font Awesome Pro 6.2.0 by @fontawesome - https://fontawesome.com License - https://fontawesome.com/license (Commercial License) Copyright 2022 Fonticons, Inc. --><path d="M64 0C28.7 0 0 28.7 0 64V448c0 35.3 28.7 64 64 64H320c35.3 0 64-28.7 64-64V160H256c-17.7 0-32-14.3-32-32V0H64zM256 0V128H384L256 0zM216 232V334.1l31-31c9.4-9.4 24.6-9.4 33.9 0s9.4 24.6 0 33.9l-72 72c-9.4 9.4-24.6 9.4-33.9 0l-72-72c-9.4-9.4-9.4-24.6 0-33.9s24.6-9.4 33.9 0l31 31V232c0-13.3 10.7-24 24-24s24 10.7 24 24z"/></svg></a>
    '
  )
}



