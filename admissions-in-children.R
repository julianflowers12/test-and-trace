library(jsonlite)

df <- fromJSON("https://coronavirus.data.gov.uk/api/v1/data?filters=areaType=nation;areaName=England&latestBy=&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22cumAdmissionsByAge%22:%22cumAdmissionsByAge%22%7D&format=json", simplifyDataFrame = TRUE)

df$data %>%
  unnest("cumAdmissionsByAge") %>%
  filter(age %in% c("0_to_5",
                    "6_to_17")) %>%
  mutate(date = lubridate::ymd(date)) %>%
  ggplot(aes(date, value, colour = age)) +
  geom_line() +
  labs(title = "Total number of children admitted to hospital with COVID since the start of the pandemic",
       y = "Admissions") +
  ggthemes::theme_economist()



 df$data %>%
  unnest("cumAdmissionsByAge") %>%
  group_by(age) %>%
  filter(age %in% c("0_to_5",
                    "6_to_17")) %>%
  mutate(date = lubridate::ymd(date), 
         daily = value - lead(value)) %>%
  ggplot(aes(date, daily, colour = age)) +
  geom_smooth(se = FALSE) +
  geom_point() +
  labs(title = "Daily number of children admitted to hospital with COVID since the start of the pandemic",
       y = "Admissions") +
  ggthemes::theme_economist()


  
