
marbles$qual <- grepl('Q', marbles$race, fixed = TRUE)


#first standardizing race...

#calculating average time by race and saving as a new df
av_race_times <- marbles %>%
  group_by(race) %>%
  dplyr::summarize(mean_race_time = mean(time_s, na.rm=TRUE))

#then repeating measures for merging
av_race_times <- av_race_times[rep(seq_len(nrow(av_race_times)), each = 16), ]

#merging with original set on id

#but first a sort
marbles<- marbles[
  with(marbles, order(race)),
]


#
marbles_new <- cbind(marbles, av_race_times$mean_race_time)

glimpse(marbles_new)

#let's rename that one...

marbles_new <- rename(marbles_new, average_by_race = `av_race_times$mean_race_time`)

marbles_new <- marbles_new[which(marbles_new$qual == TRUE),  ]

#calculating standard like they did in paper
marbles_new <- marbles_new  %>% 
  dplyr::mutate(standard_time= time_s/average_by_race)

glimpse(marbles_new)


#creating a new median variable by marble for plotting in order
med_st <- marbles_new  %>% 
  group_by(marble_name) %>%
  dplyr::summarize(med_st_time = median(standard_time, na.rm = TRUE ))

#then repeating measures for merging
med_st <- med_st[rep(seq_len(nrow(med_st)), each = 4), ]


marbles_new <- marbles_new[
  with(marbles_new, order(marble_name)),
]

glimpse(marbles_new)

marbles_new1 <- cbind(marbles_new, med_st$med_st_time)



glimpse(marbles_new1)



median(marbles_new$standard_time, na.rm = TRUE )

ggplot(marbles_new, aes(x=reorder(marble_name, med_st$med_st_time), y=standard_time)) + geom_boxplot() +
labs(title= "Boxplots of Standardized Race Times by Marble Name") + xlab("Marble Name") + ylab("Standardized Race Time") +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

flights_speed %>%
  ggplot(aes(x=reorder(carrier,-speed, na.rm = TRUE), y=speed)) +
  geom_boxplot() +
  labs(y="Speed", x="Carrier", 
       subtitle="Reordering Boxplots: In Descending Order")



marble_av_times <- marbles_new %>%
  group_by(marble_name) %>%
  dplyr::summarize(Median = median(standard_time, na.rm=TRUE))

marble_av_times <- marble_av_times[
  with(marble_av_times, order(Median)),
]
