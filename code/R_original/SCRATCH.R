
glimpse(res)

dt <- res %>% as.data.table()

dt %>% 
  drop_na(data.published.print) %>% 
  mutate(data.published.print.year = substr(data.published.print, 1, 4)) %>% 
  count(data.published.print.year) %>% 
  ggplot(aes(x = data.published.print.year, y = n)) +
  geom_col() +
  theme_classic() +
  labs(title = "Publications in Political Analysis by year")


dt %>% write_lines("./data/pan_test.csv")
View(dt %>% arrange(desc(data.published.print)))

dt %>% filter(data.title %like% "Change Comes with Time")


cr_works(dois = "10.1093/pan/mpq039") 






select(data.published.print.year) %>% sample_n(100)