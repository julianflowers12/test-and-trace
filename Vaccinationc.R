## vaccinations

vac <- read_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&metric=cumPeopleVaccinatedFirstDoseByVaccinationDate&metric=cumPeopleVaccinatedFirstDoseByPublishDate&format=csv")

vac %>%
  group_by(areaName) %>%
  mutate(total = ifelse(is.na(cumPeopleVaccinatedFirstDoseByVaccinationDate), cumPeopleVaccinatedFirstDoseByPublishDate, cumPeopleVaccinatedFirstDoseByVaccinationDate)) %>%
  gt::gt()

vac1 <- read_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&metric=weeklyPeopleVaccinatedFirstDoseByVaccinationDate&metric=newPeopleVaccinatedFirstDoseByVaccinationDate&format=csv")
vac1 %>%
  gt::gt()

