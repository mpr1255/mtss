func_get_count <- function(file_in){
  # file_in <- "10.1086_684998"
  # file_in <- "10.1093/afraf/adab005"
  # file_in <- test_paper
  # tic()
  
  if(file_in %like% "/"){
    file_in <- str_replace_all(file_in, "/", "_")
  }
  
  print(file_in)
  file_w_path <- files[file == file_in]$file_w_path
  
  if(length(file_w_path) == 0){
    print(paste0(file_w_path, " variable not filled"))
    return()
  }
  if(!file.exists(file_w_path)){
    print(paste0(file_w_path, " doesn't exist"))
    return()
  }
  
  if(file.size(file_w_path) == 0){
    print(paste0(file_w_path, " empty"))
    return()
  }
  
  this_doi <- str_replace_all(file_in, "_", "\\/")
  title <- refs2[doi == this_doi]$title
  journal <- refs2[doi == this_doi]$journal
  
  fulltext <- str_to_lower(fread(file_w_path, header = FALSE, sep = NULL))
  nchar <- nchar(fulltext)
  
  stat_terms <- tibble(term = quant_data_dict[type == "stat"]$term)
  data_terms <- tibble(term = quant_data_dict[type == "data"]$term)
  
  stat_term_count <- stat_terms %>% mutate(count = map_int(term, ~stri_count_regex(fulltext, paste0("\\b",.x,"[\\b\\W+]"))))
  data_term_count <- data_terms %>% mutate(count = map_int(term, ~stri_count_regex(fulltext, paste0("\\b",.x,"[\\b\\W+]"))))
  
  # We're aware of how this inflates the count of asterisks and it is a feature not a bug.
  asterisk_count <- quant_asterisk_dict %>% mutate(count = map(term, ~stri_count_coll(fulltext, .x)))
  
  figure_count <- as.data.table(tibble(figures = stri_count_regex(fulltext, "figure.[0-9]|figures.[0-9]|fig..[0-9]|fig.[0-9]")))
  p_value_count <- as.data.table(tibble(p_values = stri_count_regex(fulltext, paste(quant_p_value_dict, collapse = "|"))))
  
  game_formal <- as.data.table(tibble(game_formal = stri_count_regex(fulltext, paste(game_theory, collapse = "|"))))
  
  
  setDT(stat_term_count)
  setDT(data_term_count)
  
  asterisk_count[1]$term <- "ast_1"
  asterisk_count[2]$term <- "ast_2"
  asterisk_count[3]$term <- "ast_3"
  asterisk_count$count <- as.numeric(paste0(asterisk_count$count))
  setDT(asterisk_count)
  
  # stat_term_count %>% arrange(count)
  # data_term_count %>% arrange(count)
  
  t_stat_term_count <-  transpose(stat_term_count, make.names = TRUE)
  t_data_term_count <-  transpose(data_term_count, make.names = TRUE)
  t_asterisk_count <-  transpose(asterisk_count, make.names = TRUE)
  t_asterisk_count$ast_sum <- sum(t_asterisk_count)
  
  data_count <- sum(data_term_count$count) + sum(figure_count$figures)
  stat_count <- sum(stat_term_count$count) + sum(asterisk_count$count) + sum(p_value_count$p_values)
  stat_count_minus_ast <- stat_count-t_asterisk_count$ast_sum
  stat_game_ratio <- stat_count/(stat_count+game_formal$game_formal)
  
  
  res <- cbind(file_in, title, journal, t_stat_term_count, t_data_term_count, t_asterisk_count, figure_count, p_value_count, data_count = data_count, stat_count = stat_count, stat_count_minus_ast, game_formal, stat_game_ratio, nchar)
  
  return(res)
  
}