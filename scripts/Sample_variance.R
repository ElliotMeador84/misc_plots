library(tidyverse)
library(gganimate)
library(glue)

vary_pop <- function(x) {
  mean((mean(x) - x) ^ 2)
}


k <- rnorm(1000000, mean = 500, sd = 50)
#
#
df_5 <- map_df(1:10000, function(x) {
  tibble(value = sample(k, 5, T)) %>%
    mutate(sample = paste0('df.5_', x),
           size = 'Sample of 5')
  
})
#
#
df_10 <- map_df(1:10000, function(x) {
  tibble(value = sample(k, 10, T)) %>%
    mutate(sample = paste0('df.10_', x),
           size = 'Sample of 10')
  
})
#
#
df_25 <- map_df(1:10000, function(x) {
  tibble(value = sample(k, 25, T)) %>%
    mutate(sample = paste0('df.25_', x),
           size = 'Sample of 25')
  
})
#
#
df_50 <- map_df(1:10000, function(x) {
  tibble(value = sample(k, 50, T)) %>%
    mutate(sample = paste0('df.50_', x),
           size = 'Sample of 50')
  
})





df <- bind_rows(df_5, df_10, df_25, df_50)
#
df_var <- df %>%
  group_by(sample) %>%
  mutate('Sample variance' = var(value),
         'Population variance' = vary_pop(value)) %>%
  select(-value) %>%
  distinct() %>%
  gather(calculation, value, -sample, -size) %>%
  ungroup()


#
size_levs <- df_var %>%
  count(size, sort = T) %>%
  pull(size)

size.levels <- size_levs[c(3, 1, 2, 4)]

df_var_gg <- df_var %>%
  mutate(
    calculation = fct_relevel(calculation,
                              'Sample variance',
                              'Population variance'),
    size = fct_relevel(size,
                       size.levels)
  ) %>%
  filter(between(value, 0, 6000)) %>%
  group_by(size, calculation) %>%
  mutate(diverge = value - median(value)) %>%
  arrange(diverge)


# save(df_var, file = '~/df_var.RData')
# save(k, file = '~/k.RData')
#
# load('~/df_var.RData')
# load('~/k.RData')



df_median <- df_var_gg %>%
  group_by(size, calculation) %>%
  summarise(median = median(value),
            diff = round(vary_pop(k), 0) - 
                            round(median, 0)) %>%
  ungroup() %>%
  mutate(true.vary = ifelse(
    calculation == 'Population variance',
    'True\npopulation\nvariance',
    NA)) 




p <- df_var_gg %>%
  ggplot(aes(value)) +
  geom_histogram(bins = 100,
                 fill = Spectral[5],
                 show.legend = F) +
  geom_vline(xintercept = vary_pop(k),
             color = 'grey',
             size = 1.25) +
  geom_vline(
    data = df_median,
    aes(xintercept = median),
    linetype = 4,
    size = 1.25,
    color = Spectral[11]
  ) +
  geom_text(
    data = df_median,
    aes(label = paste('Difference of\n', diff), 
        x = median - 500),
    y = 475,
    size = 3) +
  geom_text(
    data = df_median,
    aes(label = true.vary),
    x = 2855,
    y = 50,
    size = 3
  ) +
  geom_text(
    data = df_median,
    aes(label = size),
    x = 3500,
    y = 350,
    size = 3.25
  )+
  scale_x_continuous(labels = scales::comma) +
  scale_fill_manual(values = YlGnBu_n(100)) +
  facet_wrap(. ~ calculation ,
             scales = 'free_x',
             ncol = 1) +
  theme_minimal()+
  theme(panel.grid.minor = element_blank(), 
        plot.caption = element_text(hjust = 0))+
  labs()



variance_p <- p + labs(title = expression(paste('Comparing sample variance ' [frac(Sigma(X - mu)^2, N - 1)], ' with population variance ' [frac(Sigma(X - mu)^2, N)])), 
         subtitle = expression(paste('The difference between '[N-1], ' and '[N], ' in stastical analysis.')),
         caption = 'I created a random normal vector of length 1,000,000 with a mean of 500 and standard deviation\nof 50.\nThen I took 10,000 random samples of size 5, 25 and 50.\nLastly, I calculated the population variance and sample variance for each to see the difference that\ndividing by N and N-1 makes.\nThe difference between the formulas for sample and population variance is small, but it makes a\nBIG difference in accuracy.\nu/lane_dog')



animated_plot <- variance_p +
  transition_states(
    size,
    transition_length = 1,
    state_length = 1,
    wrap =  T
  )



p_gganimate <- gganimate::animate(animated_plot)


anim_save(p_gganimate, filename = 'p_gganimate.gif')










