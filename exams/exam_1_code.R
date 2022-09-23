logodds_plot <- function(data, num_bins, bin_method,
                         x_name, y_name, grouping = NULL, 
                         reg_formula = y ~ x){
  
  if(is.null(grouping)){
    dat <- data.frame(x = data %>% pull(x_name), 
                      y = data %>% pull(y_name),
                      group = 1)
  } else {
    dat <- data.frame(x = data %>% pull(x_name), 
                      y = data %>% pull(y_name),
                      group = as.factor(data %>% pull(grouping)))
  }
  
  if(bin_method == "equal_size"){
    logodds_table <- dat %>%
      drop_na() %>%
      arrange(group, x) %>%
      group_by(group) %>%
      mutate(obs = y,
             bin = rep(1:num_bins,
                       each=ceiling(n()/num_bins))[1:n()]) %>%
      group_by(bin, group) %>%
      summarize(mean_x = mean(x),
                prop = mean(c(obs, 0.5)),
                num_obs = n()) %>%
      ungroup() %>%
      mutate(logodds = log(prop/(1 - prop)))
  } else {
    logodds_table <- dat %>%
      drop_na() %>%
      group_by(group) %>%
      mutate(obs = y,
             bin = cut(x, 
                       breaks = num_bins,
                       labels = F)) %>%
      group_by(bin, group) %>%
      summarize(mean_x = mean(x),
                prop = mean(c(obs, 0.5)),
                num_obs = n()) %>%
      ungroup() %>%
      mutate(logodds = log(prop/(1 - prop)))
  }
  
  if(is.null(grouping)){
    logodds_table %>%
      ggplot(aes(x = mean_x,
                 y = logodds)) +
      geom_point(size=2) +
      geom_smooth(se=F, method="lm", formula = reg_formula) +
      theme_bw() +
      labs(x = x_name,
           y = "Empirical log odds") +
      theme(text = element_text(size=15))
  } else {
    logodds_table %>%
      ggplot(aes(x = mean_x,
                 y = logodds,
                 color = group,
                 shape = group)) +
      geom_point(size=2) +
      geom_smooth(se=F, method="lm", formula = reg_formula) +
      theme_bw() +
      labs(x = x_name,
           y = "Empirical log odds",
           color = grouping,
           shape = grouping) +
      theme(text = element_text(size=15))
  }
  
}









sba <- read.csv("~/Documents/Teaching/sta214-f22.github.io/exams/sba.csv")

set.seed(23)

sba_small <- sba %>%
  slice_sample(n = 1000)

sba_small <- sba_small %>%
  filter(NewExist != 0) %>%
  mutate(Default = (MIS_Status == "CHGOFF"),
         Amount = 70000*(log(GrAppv) - min(log(GrAppv))),
         NewBusiness = ifelse(NewExist == 1, 0, 1))


logodds_plot(sba_small, 15, "equal_size", "Amount", "Default", 
             grouping = "NewBusiness",
             reg_formula = y ~ x) +
  labs(x = "Loan amount (US $)", color = "New \n Business?", 
       shape = "New \n Business?")

sba_small <- sba_small %>% 
  select(Default, Amount, NewBusiness) %>%
  drop_na()

m1 <- glm(Default ~ Amount*NewBusiness, data = sba_small,
          family = binomial)
summary(m1)


m2 <- glm(Default ~ Amount + NewBusiness, data = sba_small,
          family = binomial)
summary(m2)


m3 <- glm(Default ~ NewBusiness, data = sba_small,
          family = binomial)
summary(m3)


m4 <- glm(Default ~ Amount, data = sba_small,
          family = binomial)
summary(m4)
