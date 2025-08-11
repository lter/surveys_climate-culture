#' @title Stacked Barplot of Percent Responses by Site
#'
#' @description Creates a stacked barplot for questions with mutually-exclusive answer categories. Vertical axis is percent and horizontal is site. Answer order and color are inherited from one of the arguments
#' 
#' @param df (data.frame-like) Table of survey responses containing (at least) the following columns: 'site', 'question', 'answer', and 'percent'
#' @param focal_q (character) Question within the 'question' column of `df` for which a plot is desired
#' @param answer_colors (named character) Vector of colors where the vector names correspond exactly to the answer text for the focal question
#' 
#' @return (ggplot) Stacked barplot for the desired question
#' 
#' @importFrom magrittr %>%
#' 
plot_bar_stack <- function(df = NULL, focal_q = NULL, answer_colors = NULL){
  
  # Error checks for 'df'
  if(is.null(df) || "data.frame" %in% class(df) != TRUE || 
     all(c("site", "question", "answer", "percent") %in% names(df)) != TRUE)
    stop("'df' must be data.frame-like and contain the following columns: 'site', 'question', 'answer', and 'percent'")
  
  # Error checks for 'focal_q'
  if(is.null(focal_q) || is.character(focal_q) != TRUE || length(focal_q) != 1 || 
     focal_q %in% unique(df$question) != TRUE)
    stop("'focal_q' must be a character that exactly matches an entry in the 'question' column of 'df'")
  
  # Error checks for 'answer_colors'
  if(is.null(answer_colors) || is.null(names(answer_colors)) || is.character(answer_colors) != TRUE)
    stop("'answer_colors' must be a _named_ character vector where names correspond to entries in the 'answer' column of 'df' and values correspond to fill colors for those answers")
  
  # Prepare the data for plotting
  df_v2 <- df %>% 
    # Filter to focal question
    dplyr::filter(question == focal_q) %>% 
    # Order answers as specified by user
    dplyr::mutate(answer = factor(answer, levels = rev(names(answer_colors))))
  
  # Generate the graph
  p <- ggplot(df_v2, aes(x = site, y = percent, fill = answer, color = "x")) +
    geom_bar(stat = "identity") +
    scale_color_manual(values = "#000") +
    scale_fill_manual(values = ord) +
    labs(y = "Percent Responses") +
    guides(color = "none") +
    theme_bw() +
    theme(legend.position = "right",
          legend.title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 10))
  
  # Return the graph
  return(p) }

# End ----
