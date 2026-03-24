library(tidyverse)

# call_data <- read.csv("https://git.new/ECRF6iH")
# call_data <- read_csv("call_data.csv")

head(call_data)

# plot the data
call_data %>% 
  ggplot(., aes(x = time_to_pause, y = TEC)) + geom_point()

# aggregate the data
agg_df <- call_data %>% 
  group_by(time_to_pause) %>% 
  summarise(TEC_mean = mean(TEC))
head(agg_df)

# try plotting again
agg_df %>% 
  ggplot(., aes(x = time_to_pause, y = TEC_mean)) + geom_point()

# let's limit to within an hour. Can change this later
new_df <- call_data %>% 
  filter(time_to_pause <=60 & time_to_pause >=-60)
agg_df <- new_df %>% 
  group_by(time_to_pause) %>% 
  summarise(TEC_mean = mean(TEC))
agg_df %>% 
  ggplot(., aes(x = time_to_pause, y = TEC_mean)) + geom_point()

# lets add a line
agg_df %>% 
  ggplot(., aes(x = time_to_pause, y = TEC_mean)) + geom_point() +
  stat_smooth(method = "lm")

# lets try it at the discontinuity
agg_df %>% 
  ggplot(., aes(x = time_to_pause, y = TEC_mean)) + geom_point()+
  stat_smooth(data=. %>% filter(time_to_pause < -0), method = "lm") +
  stat_smooth(data=. %>% filter(time_to_pause >=-0), method = "lm")

# let's make some better lines with regressions
reg_df <- new_df %>% 
  mutate(z = if_else(time_to_pause >=0,1,0)) %>% 
  mutate(interact1 = z*time_to_pause) %>% 
  mutate(t2 = time_to_pause^2) %>% 
  mutate(interact2 = t2*z) %>% 
  mutate(t3 = time_to_pause^3) %>% 
  mutate(interact3 = z*t3) %>% 
  filter(time_to_pause <=-3 | time_to_pause >=0)

r1 <- lm(TEC ~ z + time_to_pause + interact1, data = reg_df)
tidy(r1)
# get predicted values
plot_df <- reg_df %>% 
  mutate(y_pred = predict(r1, newdata=.))

# put into graph
agg_df %>% 
  ggplot(., aes(x = time_to_pause, y = TEC_mean)) + geom_point() +
  geom_line(data = plot_df, aes(x = time_to_pause, y = y_pred))

# we have to do this seperately
# put into graph
agg_df %>% 
  ggplot(., aes(x = time_to_pause, y = TEC_mean)) + geom_point() +
  geom_line(data = plot_df %>% filter(z==0), aes(x = time_to_pause, y = y_pred)) +
  geom_line(data = plot_df %>% filter(z==1), aes(x = time_to_pause, y = y_pred)) 

# this doesn't fit so well
# let's try more
r2 <- lm(TEC ~ z + time_to_pause + interact1 + t2 + interact2, data = reg_df)
tidy(r2)
r3 <- lm(TEC ~ z + time_to_pause + interact1 + t2 + interact2 + t3 + interact3, data = reg_df)
tidy(r3)

plot_df <- reg_df %>% 
  mutate(y_pred1 = predict(r1, newdata=.)) %>% 
  mutate(y_pred2 = predict(r2, newdata=.)) %>% 
  mutate(y_pred3 = predict(r3, newdata=.))

agg_df %>% 
  ggplot(., aes(x = time_to_pause, y = TEC_mean)) + geom_point() +
  geom_line(data = plot_df %>% filter(z==0), aes(x = time_to_pause, y = y_pred1), colour = "red") +
  geom_line(data = plot_df %>% filter(z==1), aes(x = time_to_pause, y = y_pred1), colour = "red")+
  
  geom_line(data = plot_df %>% filter(z==0), aes(x = time_to_pause, y = y_pred2), colour = "goldenrod") +
  geom_line(data = plot_df %>% filter(z==1), aes(x = time_to_pause, y = y_pred2), colour = "goldenrod")+
  
  geom_line(data = plot_df %>% filter(z==0), aes(x = time_to_pause, y = y_pred3), colour = "cornflowerblue") +
  geom_line(data = plot_df %>% filter(z==1), aes(x = time_to_pause, y = y_pred3), colour = "cornflowerblue") +
  theme_minimal()

# let's look at the quadratic regression again
tidy(r2)

#### Balance ####

agg_df <- new_df %>% 
  group_by(time_to_pause) %>% 
  summarise(TEC_mean = mean(TEC), urban_mean = mean(urban), white_mean = mean(white), 
            priority1 = mean(priority1), male_mean= mean(male))

agg_df %>% 
  ggplot(., aes(x = time_to_pause, y = urban_mean)) + geom_point()+
  stat_smooth(data=. %>% filter(time_to_pause < -0), method = "lm") +
  stat_smooth(data=. %>% filter(time_to_pause >=-0), method = "lm")

agg_df %>% 
  ggplot(., aes(x = time_to_pause, y = white_mean)) + geom_point()+
  stat_smooth(data=. %>% filter(time_to_pause < -0), method = "lm") +
  stat_smooth(data=. %>% filter(time_to_pause >=-0), method = "lm")

agg_df %>% 
  ggplot(., aes(x = time_to_pause, y = priority1)) + geom_point()+
  stat_smooth(data=. %>% filter(time_to_pause < -0), method = "lm") +
  stat_smooth(data=. %>% filter(time_to_pause >=-0), method = "lm")

agg_df %>% 
  ggplot(., aes(x = time_to_pause, y = male_mean)) + geom_point()+
  stat_smooth(data=. %>% filter(time_to_pause < -0), method = "lm") +
  stat_smooth(data=. %>% filter(time_to_pause >=-0), method = "lm")

# make a table

#### Sorting ####

count_df <- call_data %>% 
  group_by(time_to_pause) %>% 
  summarise(n = n())
head(count_df)

count_df %>% 
  ggplot(., aes(x = time_to_pause, y = n)) + geom_bar(stat = "identity")

#### ED Outcome ####

# ED within 3 days
agg_df <- new_df %>% 
  group_by(time_to_pause) %>% 
  summarise(TEC_mean = mean(TEC), ED3_mean = mean(ED3))
head(agg_df)

agg_df %>% 
  ggplot(., aes(x = time_to_pause, y = ED3_mean)) + geom_point()

reg1 <- lm(ED3 ~ z + time_to_pause + interact1, data = reg_df)
tidy(reg1)
reg2 <- lm(ED3 ~ z + time_to_pause + interact1 + t2 + interact2, data = reg_df)
tidy(reg2)
reg3 <- lm(ED3 ~ z + time_to_pause + interact1 + t2 + interact2 + t3 + interact3, data = reg_df)
tidy(reg3)

plot_df <- reg_df %>% 
  mutate(y_pred1 = predict(reg1, newdata=.)) %>% 
  mutate(y_pred2 = predict(reg2, newdata=.)) %>% 
  mutate(y_pred3 = predict(reg3, newdata=.))

agg_df %>% 
  ggplot(., aes(x = time_to_pause, y = ED3_mean)) + geom_point() +
  geom_line(data = plot_df %>% filter(z==0), aes(x = time_to_pause, y = y_pred1), colour = "red") +
  geom_line(data = plot_df %>% filter(z==1), aes(x = time_to_pause, y = y_pred1), colour = "red")+
  
  geom_line(data = plot_df %>% filter(z==0), aes(x = time_to_pause, y = y_pred2), colour = "goldenrod") +
  geom_line(data = plot_df %>% filter(z==1), aes(x = time_to_pause, y = y_pred2), colour = "goldenrod")+
  
  geom_line(data = plot_df %>% filter(z==0), aes(x = time_to_pause, y = y_pred3), colour = "cornflowerblue") +
  geom_line(data = plot_df %>% filter(z==1), aes(x = time_to_pause, y = y_pred3), colour = "cornflowerblue") +
  theme_minimal()

tidy(reg2)

#### 2SLS #### 

# for estimate, can just divide
reg2$coefficients[2]/r2$coefficients[2]

# for standard error, a few methods depending on data