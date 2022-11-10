library(nnet)

books <- read.csv("https://sta712-f22.github.io/homework/books.csv") %>%
  mutate(age = 2022 - Year) %>%
  drop_na() %>%
  mutate(genre = factor(genre, levels = c("Other", "General", "Fantasy", 
                                          "ComicsGraphicNovel","Business",
                                          "Mystery", "Fiction"))) %>%
  mutate(purchases)

m1 <- multinom(genre ~ age + page_count + price + rating + purchases,
               data = books)
summary(m1)

table("predicted" = predict(m1), "actual" = books$genre) %>%
  xtable::xtable()


m2 <- glm(purchases ~ genre + age + page_count + price + rating,
          data = books, family = poisson)
summary(m2)

m3 <- glm(purchases ~ genre + age + page_count + price + rating,
          data = books, family = quasipoisson)
summary(m3)


m3 <- zeroinfl((purchases-1) ~ genre + Year + page_count + price + rating |
                 genre + Year + page_count + price + rating,
               data = books)
summary(m3)
