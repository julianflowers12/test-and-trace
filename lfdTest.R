library(ukcovid19)

filters <- "areaType=utla"
structure <- list(

       date = "date",
       code = "areaCode",
       name = "areaName",
       newLFD = "newLFDTests", 
       cumLFD = "cumLFDTests"

)

lfdTests <- get_data(

    filters = filters, 
    structure = structure
)

lfdTests  %>% 
      mutate(date = lubridate::ymd(date)) %>%
      ggplot(aes(date, newLFD, fill = name)) +
      geom_col(show.legend = FALSE)

