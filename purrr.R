library(tidyverse)
data(mtcars)
mtcars %>% 
  head

for (i in 2:5) {
  assign(paste0("kmeans_seg", i),
         kmeans(mtcars, i))
}

result <- tibble(segment = 2:5) %>% 
  mutate(kmean = purrr::map(.x = segment,
                            .f = ~kmeans(mtcars, .x))) 



