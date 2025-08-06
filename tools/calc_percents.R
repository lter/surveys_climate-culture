#' @title Calculate Percent Responses for a Particular Question
#' 
#' @description Quickly calculate percent responses for a survey question. Allows filtering to a particular answer/answers or returning percentages of all response levels. Automatically groups by "site" column.
#' 
#' @param df (data.frame-like) Survey response data
#' @param df (character) Column name corresponding to question to summarize
#' 
#' @importFrom magrittr %>%
#' 
#' @return (data.frame) Wide-format dataframe of percents per category of answer
#' 
#' 
calc_percents <- function(df = NULL, q = NULL){
  
  # Error checks for 'df'
  if(is.null(df) || "data.frame" %in% class(df) != TRUE || "site" %in% names(df) != TRUE)
    stop("'df' must be provided, be data.frame-like, and contain a 'site' column")
  
  # Error checks for 'q'
  if(is.null(q) || is.character(q) != TRUE || length(q) != 1 || q %in% names(df) != TRUE)
    stop("'q' must be a single column name found in 'df'")
  
  # Calculate desired metric for input data
  df_v2 <- df %>% 
    # Pare down to bare minimum columns
    dplyr::select(site, dplyr::contains(q)) %>%
    # Remove missing values
    dplyr::filter(!is.na(!!rlang::ensym(q)) & nchar(!!rlang::ensym(q)) != 0) %>% 
    # Get 'total responses' per site
    dplyr::group_by(site) %>% 
    dplyr::mutate(total = dplyr::n()) %>% 
    dplyr::ungroup() %>% 
    # Get response per category
    dplyr::group_by(site, !!rlang::ensym(q), total) %>% 
    dplyr::summarize(ct = dplyr::n(),
                     .groups = "keep") %>% 
    dplyr::ungroup() %>% 
    # Calculate percents
    dplyr::mutate(percent = (ct / total) * 100) %>% 
    # Generate more specific question column
    dplyr::mutate(question = paste0(q, "_", tolower(!!rlang::ensym(q)), "_percent")) %>% 
    # Reorder/pare down
    dplyr::select(site, question, total, ct, percent)
    
  # Return that object
  return(df_v2) }

# End ----
