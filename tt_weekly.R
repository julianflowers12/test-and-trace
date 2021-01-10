### test and trace weekly reports 2

link <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/948744/NHS_T_T_data_tables_w30.ods"

temp <- tempfile()

downloader::download(link, temp)

tt_data <- readODS::read_ods(temp)

tt_sheets <- readODS::list_ods_sheets(temp)

tt_results <- readODS::read_ods(temp, sheet = "Table_4", skip = 2) %>%
    data.frame() %>%
    slice(1:24)

options(scipen = 999)

tt_results_1 <- tt_results %>%
  janitor::remove_empty() %>%
  slice(-c(2, 11)) %>%
  pivot_longer(names_to = "period", values_to = "val", cols = 2:ncol(.)) %>%
  mutate(row = row_number()) %>%
  mutate(val = as.numeric(val)) %>%
  na.omit() %>%
  mutate(period = str_extract(period, "X\\d{2}.\\d{2}.\\d{2}"), 
         period = str_remove(period, "X"), 
         period = lubridate::dmy(period))


tt_results_1 %>%
  filter(str_detect(Var.1, "[Pp]ercentage"), 
         str_detect(Var.1, "taking")) %>%
  mutate(metric = fct_relevel(Var.1, "Percentage of test results received within 24 hours of taking a test", "Percentage of test results received between 24 hours and 48 hours of taking a test",
                              "Percentage of test results received between 48 hours and 72 hours of taking a test")) %>%
  ggplot(aes(period, val, group = metric, fill = Var.1)) +
  geom_col(position = "fill") +
  viridis::scale_fill_viridis(discrete = TRUE)


+
  facet_wrap(~Var.1, labeller = label_wrap_gen(25))

 