
clients <- read.csv("/Users/chris/Downloads/DataFest 2023 data/clients.csv")
attorneys <- read.csv("/Users/chris/Downloads/DataFest 2023 data/attorneys.csv")

library(dplyr)

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

state_counts = state_counts[,-3]
state_counts = state_counts[-10,]
state_counts$client_to_attorney_ratio = state_counts$Clients/state_counts$Attorneys

write.csv(state_counts, "/Users/chris/Downloads/state_counts.csv", row.names=TRUE)

attorneytimes <- read.csv("/Users/chris/Downloads/DataFest 2023 data/attorneytimeentries.csv")

grouped_times <- attorneytimes %>%
  group_by(StateAbbr)

grouped_times <- grouped_times %>%
  select(-c(AttorneyUno, TimeEntryUno))

avg_hours_per_state <- grouped_times %>%
  group_by(StateAbbr) %>%
  summarise(avg_hours = mean(Hours))

avg_hours_per_state$state_name = c("Alaska","Alabama","Arkansas","Arizona","California","Connecticut","Florida","Georgia","Hawaii","Iowa","Illinois","Indiana","Kansas","Louisiana","Massachusetts","Maryland","Maine","Michigan","Missouri","Mississippi","North Carolina","Nebraska","New Hampshire",
                                  "New Jersey","New Mexico","New York","Oklahoma","Pennsylvania","South Carolina","South Dakota","Tennessee","Texas","US","Utah","Virginia","Vermont","Wisconsin","West Virginia","Wyoming")

colnames(state_counts)[1] = "state_name"

states = merge(state_counts, avg_hours_per_state,by="state_name")

cor(states$client_to_attorney_ratio,states$avg_hours)
plot(states$client_to_attorney_ratio, states$avg_hours, ylab = "Average hours spent responding to client questions", xlab = "Client to attorney ratio")

write.csv(states, "/Users/chris/Downloads/states.csv", row.names=TRUE)

questions = read.csv("/Users/chris/Downloads/DataFest 2023 data/questions.csv")

library(tidyr)
library(dplyr)
library(readr)

questions$count_one = 1
questions_category = questions %>% group_by(Category, StateAbbr) %>% summarise(category_counts=sum(count_one))

Test10 = questions_category %>% pivot_wider(
  names_from = Category,
  values_from = category_counts
)

final = read.csv("/Users/chris/Downloads/clients-csv.csv")
final$count_one = 1
final_adj = EthnicCount = final %>% 
  count(StateName, EthnicIdentity, name = "CountEthPerSt")

DataFest = final_adj %>% pivot_wider(
  names_from = EthnicIdentity,
  values_from = CountEthPerSt
)
DataFest = DataFest[-1,]

Prime = merge(DataFest, Test10,by="StateName")
Test10 = Test10[-40,]
Test10$StateName = c("Alaska","Alabama","Arkansas","Arizona","California","Connecticut","Florida","Georgia","Hawaii","Iowa","Illinois","Indiana","Kansas","Louisiana","Massachusetts","Maryland","Maine","Michigan","Missouri","Mississippi","North Carolina","Nebraska","New Hampshire",
                                   "New Jersey","New Mexico","New York","Oklahoma","Pennsylvania","South Carolina","South Dakota","Tennessee","Texas","Utah","Virginia","Vermont","Wisconsin","West Virginia","Wyoming", "Idaho")
Prime=Prime[,-10]
colnames(Prime)[8] = "Race_other"
colnames(Prime)[9] = "Race_NA"
colnames(Prime)[18] = "Status_other"

GenderCount = final %>%
  count(StateName, Gender, name= "CountGenderPerSt")
Gender = GenderCount %>% pivot_wider(
  names_from = Gender,
  values_from = CountGenderPerSt
)
colnames(Gender)[5] = "Gender_NA"
Gender = Gender[-1,]
Prime = merge(Prime, Gender,by="StateName")


StatusCount = final %>%
  count(StateName, MaritalStatus, name= "CountMarStatusPerSt")

Status = StatusCount %>% pivot_wider(
  names_from = MaritalStatus,
  values_from = CountMarStatusPerSt
)
colnames(Status)[6] = "Status_NA"
Status = Status[-1,]
Prime = merge(Prime, Status,by="StateName")


VeteranCount = final %>%
  count(StateName, Veteran, name= "CountVetsPerSt")

Veteran = VeteranCount %>% pivot_wider(
  names_from = Veteran,
  values_from = CountVetsPerSt
)
colnames(Veteran)[4] = "Veteran_NA"
Veteran = Veteran[-1,]
Prime = merge(Prime, Veteran,by="StateName")


ImprisonedCount = final %>%
  count(StateName, Imprisoned, name= "CountImprisonedPerSt")

Imprisoned = ImprisonedCount %>% pivot_wider(
  names_from = Imprisoned,
  values_from = CountImprisonedPerSt
)
colnames(Imprisoned)[4] = "Imprisoned_NA"
Imprisoned = Imprisoned[-1,]
Prime = merge(Prime, Imprisoned,by="StateName")

colnames(Prime)[29] = "Veteran_No"
colnames(Prime)[30] = "Veteran_Yes"

colnames(Prime)[32] = "Imprisoned_No"
colnames(Prime)[33] = "Imprisoned_Yes"

final_update = read.csv("/Users/chris/Downloads/clients-csv (1).csv")
median_income = final_update %>% group_by(StateName) %>% summarise(median_income = median(na.omit(AnnualIncome)))
median_income = median_income[-1,]
Prime = merge(Prime, median_income,by="StateName")

write.csv(Prime, "/Users/chris/Downloads/Prime.csv", row.names=TRUE)

Prime = read.csv("/Users/chris/Downloads/Prime.csv")
Prime = Prime[-10,]
Prime = Prime[,-1]

ratio = state_counts[,-c(2,3)]
colnames(ratio)[1] = "StateName"
Prime = merge(Prime, ratio,by="StateName")

hours = avg_hours_per_state[,-1]
hours = hours[-33,]
colnames(hours)[2] = "StateName"

Prime = merge(Prime, hours,by="StateName")
write.csv(Prime, "/Users/chris/Downloads/Prime.csv", row.names=TRUE)

cor(Prime$avg_hours,Prime$median_income)
cor(Prime$avg_hours,Prime$client_to_attorney_ratio)



