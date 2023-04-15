install.packages('ggplot2')
library(ggplot2)

clients <- read.csv("C:/Users/Ryan/Downloads/DataFest 2023 data/clients.csv")
attorneys <- read.csv("C:/Users/Ryan/Downloads/DataFest 2023 data/attorneys.csv")
attorneytimes <- read.csv("C:/Users/Ryan/Downloads/DataFest 2023 data/attorneytimeentries.csv")
questions <- read.csv('C:/Users/Ryan/Downloads/DataFest 2023 data/questions.csv')

grouped_data <- clients %>% 
  group_by(StateName)

clients_counted <- grouped_data %>% 
  count()

clients_counted <- clients_counted %>% 
  rename(Clients = n)

grouped_data1 <- attorneys %>% 
  group_by(StateName)

attorneys_counted <- grouped_data1 %>% 
  count()

attorneys_counted <- attorneys_counted %>% 
  rename(Attorneys = n)

state_counts <- cbind(clients_counted[-1,], attorneys_counted[-c(1,29,36),])

grouped_times <- attorneytimes %>% 
  group_by(StateAbbr)

grouped_times <- grouped_times %>% 
  select(-c(AttorneyUno, TimeEntryUno))

avg_hours_per_state <- grouped_times %>% 
  group_by(StateAbbr) %>% 
  summarise(avg_hours = mean(Hours))


grouped_qs <- questions %>% 
  group_by(StateAbbr)

grouped_qs <- grouped_times3 %>% 
  select(c(StateAbbr, Category))

qcounts <- aggregate(Category ~ StateAbbr, data = grouped_times3, FUN = mode)

print(qcounts)

Prime <- read.csv('C:/Users/Ryan/Downloads/Prime (1).csv')
dev.off()

ggplot(Prime, aes(x = Prime$avg_hours, y = Prime$median_income, color = StateName)) +
  geom_point() +
  xlab("Hours") +
  ylab("Annual Salary") +
  ggtitle("Hours vs. Annual Salary by State") +
  annotate("text", x=1.11, y=28000, label="r = -0.065")

ggplot(Prime, aes(x = Prime$avg_hours, y = Prime$client_to_attorney_ratio, color = StateName)) +
  geom_point() +
  xlab("Hours") +
  ylab("Client:Attorney Ratio") +
  ggtitle("Average Hours vs. Client:Attorney Ratio by State") +
  annotate("text", x=1.1, y=80, label = "r = -0.077")
  



