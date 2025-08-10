

## DELETE ME (vvv) ----
# Placeholders for arguments
df <- res_v2
focal_q <- "fieldwork_duration"
answer_colors <- c("Other" = "gray80",
                   "0 weeks" = "#e9d8a6",
                   "1-3 weeks" = "#ee9b00",
                   "1-3 months" = "#bb3e03",
                   "Longer" = "#540b0e")
## DELETE ME (^^^) ----



# Error checks for 'df'
if(is.null(df) || "data.frame" %in% class(df) != TRUE || 
   all(c("question", "answer") %in% names(df)) != TRUE)
  stop("'df' must be data.frame-like and contain the following columns: 'question' and 'answer'")

# Error checks for 'focal_q'
if(is.null(focal_q) || is.character(focal_q) != TRUE || length(focal_q) != 1 || 
   focal_q %in% unique(df$question) != TRUE)
  stop("'focal_q' must be a character that exactly matches an entry in the 'question' column of 'df'")

# Error checks for 'answer_colors'
if(is.null(answer_colors) || is.null(names(answer_colors)) || is.character(answer_colors) != TRUE)
  stop("'answer_colors' must be a _named_ character vector where names correspond to entries in the 'answer' column of 'df' and values correspond to fill colors for those answers")




# HERE NOW ----


# Prepare data
df_v2 <- df %>% 
  # Filter to focal question
  dplyr::filter(question == focal_q) %>% 
  # Order answers as specified by user
  dplyr::mutate(answer = factor(answer, levels = rev(names(ord))))



# Identify preferred order & colors

# Prepare data
df_prep <- res_v2 %>% 
  dplyr::filter(question == "fieldwork_duration") %>% 
  dplyr::mutate(answer = factor(answer, levels = rev(names(ord))))

# Check structure
dplyr::glimpse(df_prep)

# Make a network-wide version
ggplot(df_prep, aes(x = site, y = percent, fill = answer, color = "x")) +
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
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1),
        axis.text.y = element_text(size = 10))

# Export locally
ggsave(filename = file.path("graphs", "fieldwork_duration__network.png"),
       height = 4, width = 8, units = "in")




# End ----
