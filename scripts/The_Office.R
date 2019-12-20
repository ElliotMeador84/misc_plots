library(tidyverse)
library(schrute)
library(glue)
library(ggrepel)



she_said <- theoffice %>% 
  transmute(she_said = ifelse(
    str_detect(text, 'what she said'), 1, 0), 
    season = parse_number(season), 
    episode = parse_number(episode), 
    text.ii = str_trim(text),
    text.i = lead(text),
    text.iii = lag(text), 
    character,
    ) %>% 
  filter(she_said == 1) %>% 
  mutate(text_all = glue('{text.i} - {text.ii} [{character}]'))


alfa <- she_said %>% 
  mutate(season_episode = glue('{season}.{episode}'), 
         season_episode = as.numeric(season_episode)) %>% 
  arrange(season_episode) %>% 
  select(-episode)



 
tango <- alfa %>%
  group_by(season_episode) %>% 
  mutate(total = sum(she_said)) %>% 
  ungroup() %>% 
  distinct(season_episode, total, .keep_all = T) %>% 
  mutate(cumsum = cumsum(total), 
         season = as.factor(season)) 




she_said_plot <- tango  %>% 
  ggplot(aes(season_episode, cumsum, group = season))+
  geom_line(aes(color = season), 
            show.legend = F)+
  geom_point(aes(color = season), 
             size = 3, 
             show.legend = F)+
  geom_text_repel(aes(label = str_wrap(text_all, 25), 
                      color = season), 
                  size = 1.5, 
                  segment.colour = 'black', box.padding = .5,
                  arrow = arrow(type = 'closed', 
                                length = unit(1.25, 'mm')),
                  show.legend = F)+
  scale_x_continuous(breaks = 1:9, labels = as.character(1:9))+
  expand_limits(x = 1)+
  scale_y_continuous(breaks = seq(0, 25, 5))+
  scale_color_brewer(palette = 'Dark2')+
  theme_minimal()+
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(linetype = 4))+
  labs(title = 'The Office (US)', 
       subtitle = 'Looking at the frequency of \"That\'s what she said!\" jokes', 
       x = 'Seasons', 
       y = 'Cumulative growth of\n"That\'s what she said!\" jokes')+
  coord_fixed(1/6)



ggsave(she_said_plot, 
       width = 11, 
       height = 8, 
       filename = 'png/she_said_plot.pdf')












