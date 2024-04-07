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
result$kmean

df <- tibble(A = rnorm(mean = 0, sd=1, n= 100),
             B = rnorm(mean = 1, sd = 2, n= 100),
             Location = rep(c("Tokyo", "Osaka"), each = 50) %>% factor)
df %>% 
  slice_sample(n=5)

df %>% 
  group_by(Location) %>% 
  nest(data=c(A,B))

df %>% 
  group_by(Location) %>% 
  nest(data=c(A,B)) %>% 
  mutate(LM = map(.x = data,
                  .f = ~lm(data = .x,
                           A~B)))
kmeansAIC <- function(fit){
  m <- ncol(fit$centers)
  n <- length(fit$cluster)
  k <- nrow(fit$centers)
  D <- fit$tot.withinss
  return(AIC = D + 2*m*k)
}
kmeans(mtcars, 2) %>% 
  kmeansAIC()

tibble(segment = 2:5) %>% 
  mutate(kmean = purrr::map(.x = segment,
                            .f = ~kmeans(mtcars, .x))) %>% ### ここまではさっきと同じ
  mutate(AIC = purrr::map_dbl(.x = kmean,
                              .f = ~kmeansAIC(.x)))
tibble(segment = 2:5) %>% 
  mutate(kmean = purrr::map(.x = segment,
                            .f = ~kmeans(mtcars, .x))) %>% ### ここまではさっきと同じ
  mutate(AIC = purrr::map(.x = kmean,
                          .f = ~kmeansAIC(.x)))


hoge <- df %>% 
  group_by(Location) %>% 
  nest(data=c(A,B)) %>% 
  mutate(LM = map(.x = data,
                  .f = ~lm(data = .x,
                           A~B)))
hoge

make_scatter <- function(df, name){
  ggplot(data = df,
         mapping = aes(x = A,
                       y = B))+
    geom_point()+
    labs(title = paste0(name, "におけるAとBの散布図"))+
    theme(text = element_text(family ="HiraKakuPro-W3"))
}

hoge <- hoge %>% 
  mutate(gg = map2(.x = data,
                   .y = Location,
                   .f = ~make_scatter(df=.x,
                                      name=.y)))

hoge$gg

library(patchwork)
hoge$gg[[1]] + hoge$gg[[2]]

demographics <- tibble(A = rnorm(mean = 0, sd=1, n= 100),
                       B = rnorm(mean = 1, sd = 2, n= 100),
                       Location = rep(c("Tokyo", "Osaka"), each = 50) %>% factor,
                       age = rnorm(n=100, mean=30, sd=10) %>% round(-1) %>% as.integer()) 
demographics


nested_demographics <- demographics %>% 
  group_by(Location,age) %>% 
  nest(data = c(A,B))

nested_demographics

make_scatter2 <- function(first, second, third){
  return(ggplot(data = first,
                mapping = aes(x = A,
                              y = B))+
           geom_point()+
           labs(title = paste0(second,"かつ",third,"年代のAとBの散布図"))+
           theme(text = element_text(size=12))+
           theme(text = element_text(family ="HiraKakuPro-W3"))
  )
}

nested_demographics <- nested_demographics %>% 
  mutate(gg = pmap(.l = list(first = data,
                             second = Location,
                             third = age),
                   .f = make_scatter2)) # ここで引数は指定せず、チルダも不要
nested_demographics

nested_demographics$gg[[1]]


