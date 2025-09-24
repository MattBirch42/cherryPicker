library(lubridate)
library(dplyr)

n <- 1000
end_date <- ymd("2025-09-23")

set.seed(42)

fake_data <- data.frame(
  row_index = seq(1,n,1),
  date = seq.Date(end_date - days(n - 1),end_date,1)) %>%
  mutate(cycle_var = 50 + 30*sin(as.numeric(date)/200) + rnorm(n,2,2),
         noise = rnorm(n,35,4),
         odd_days = case_when(day(date) %% 2 == 0 ~ 0,
                              TRUE ~ 1),
         year = year(date),
         month = month(date),
         string = sample(c("orange", "pizza", "cat"), replace = T,n)) %>%
  mutate(
    mid = median(as.numeric(date)),
    dist = (as.numeric(date) - mid)^2,
    smile = {
      dist_scaled <- dist / max(dist)   
      -70 + (50 - (-70)) * dist_scaled  
    }
  ) %>%
  select(-mid, -dist) %>%
  {
    .$noise[sample(n, 2)] <- NA
    .$smile[sample(n, 3)] <- NA
    .$cycle_var[sample(n, 4)] <- NA
    .
  } %>%
  {
    numeric_cols <- setdiff(names(.), c("year", "month", "string", "date", "row_index"))
    for (col in numeric_cols) {
      idx <- sample(which(!is.na(.[[col]])), 2) 
      .[[col]][idx] <- .[[col]][idx]^2
    }
    .
  } %>%
  {
    idx <- sample(n, 1)
    .$string[idx] <- "your_opinion_is_wrong"
    .
  }

write.csv(fake_data,"fake_data1.csv", row.names = F)
