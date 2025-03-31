library("tidyverse")

data1 <- read.csv("../data/Crash_Analysis_System_(CAS)_data.csv")
col <- colnames(data1)
selected <- c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)

cols <- c()
for (i in 1:length(col)){
  if (selected[i] == TRUE){
    print(col[i])
    cols <- append(cols, col[i])
  }
}
data2 <- data1 %>% select(all_of(cols)) 

data2 <- data2 %>% mutate(weather = case_when(weatherA != "Null" & weatherB != "Null" & weatherB != "None"~ paste(weatherA, "&", weatherB), weatherA != "Null" ~ weatherA, TRUE ~ "Null")) %>% filter(weather != "Null") %>% select(!all_of(c("weatherA", "weatherB")))

write.csv(data2, "../data/clean_data.csv")