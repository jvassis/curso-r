rm(list= ls())

library(tidyverse)
library(janitor)
library(scales)
library(stringr)
library(ggtext)


text = "
  item,Bottom box,Middle,Top box
  Survey item A,1%,66%,33%
  Survey item B,5%,83%,12%
  Survey item C,8%,83%,9%
  Survey item D,11%,80%,9%
"

df = text %>%
  read.csv2(text=., sep=',') %>%
  clean_names() %>%
  pivot_longer(-item) %>%
  mutate(item = str_trim(item)) %>%
  mutate(
    value = parse_number(value)/100,
    label = if_else(name=='middle', '', percent(value)),
    hjust = runif(n()),
    cor = if_else(item == 'Survey item A' & name == 'bottom_box', '#c3514e', 'white')
  )

sub_title = "
<strong>
<span style='color:#c3514e'>Strongly Disagree</span>
<span style='color:#bfbebe'> | Disagree | Neutral | </span>
<span style='color:#4bacc6'>Strongly Agree</span>
</strong>
"

df %>%
  ggplot() +
  aes(value, fct_rev(item), fill = fct_rev(name)) +
  geom_col() +
  geom_text(
    aes(label = label),
    color = 'white',
    position = position_stack(vjust = .5),
    data = filter(
      df,
      !(item == 'Survey item A' & name == 'bottom_box')
    )
  ) +
  geom_text(
    aes(label = label),
    color = '#c3514e',
    nudge_x = .04,
    data = filter(
      df,
      item == 'Survey item A' & name == 'bottom_box'
    )
  ) +
  scale_x_continuous(
    labels = percent,
    breaks = seq(0,1,.2),
    position = 'top',
    expand = c(0,0)
  ) +
  annotate(
    'text', x=1,y=4.2,hjust=-.1,
    label='Survey team A', color='#4bacc6'
  ) +
  annotate(
    'text', x=1,y=3.8,hjust=-.1,
    label='ranked highest\n for team X', color='#bfbebe'
    
  ) +
  annotate(
    'text', x=1,y=1,hjust=-.1,
    label='Disssatisfaction\n was greatest\n for ', color='#bfbebe'
  ) +
  annotate(
    'text', x=1,y=.75,hjust=-.35,
    label='Survey team A', color='#c3514e'
    
  ) +
  coord_cartesian(clip='off', xlim=c(0,1.5)) +
  scale_fill_manual(
    values=c('#4bacc6','#bfbebe','#c3514e')
  ) +
  theme_minimal() +
  labs(
    title='Survey Results: Team X',
    subtitle = sub_title
  ) +
  theme(
    panel.grid = element_blank(),
    axis.line.x.top = element_line(size=.1, colour='gray20'),
    axis.ticks.x.top = element_line(size=.5, colour='gray20'),
    axis.text.y = element_text(colour='black', size=13),
    legend.position = 'none',
    axis.title = element_blank(),
    plot.title.position = 'plot',
    plot.subtitle = element_markdown(size=14,hjust=.5)
  )


























