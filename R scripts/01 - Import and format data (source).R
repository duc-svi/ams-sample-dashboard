#Import data----
sample <- read_excel(here("Quarto outputs", "Data", "Sample data_20261012.xlsx"))

#Clean data----
##Report month----
sample <- sample |>
  mutate(report_month = floor_date(report_month, "month"))

##Ingredients----
sample <- sample |>
  mutate(ingredient = case_when(ingredient == "cefotaxim" ~ "Cefotaxim",
                                .default = ingredient))

##Unit----
sample <- sample |>
  mutate(unit = case_when(unit == "chai" ~ "bottle",
                          unit == "Óng" ~ "tube",
                          .default = unit))
