#' HTML Likert scale
#'
#' @param question
#' @param response_options
#' @param scale_name
#' @param extra_css
#'
#' @return
#' @export
#'
#' @examples
likert <- function(question, response_options, scale_name = "likert", extra_css = "") {

  items <- ""
  for(i in seq_along(response_options)) {
    items <- stringr::str_c(items, htmltools::HTML(glue::glue('
      <label>
        <input class="form-check-input" type="radio" name="{scale_name}" style="margin-left: 1em;" {ifelse(i == ceiling(length(response_options)/2), "checked", "")}>
        {response_options[i]}
      </label><br>
    ')))
  }

  htmltools::HTML(glue::glue('
    <div style="border: 1px solid; border-radius: 5px; padding: 1em; {extra_css}">
    <p style="font-weight: bold;">{question}</p>
    {items}
    </div>
'))
}
