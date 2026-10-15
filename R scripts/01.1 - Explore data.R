#Explore data----
summary(sample)

sample |> count(ingredient, sort = TRUE)

sample |> count(weight_gram, sort = TRUE)

sample |> count(unit, sort = TRUE)

sample |> count(quantity, sort = TRUE)

str(sample$quantity)

sample |> count(report_month, sort = TRUE)

sample |>
  group_by(report_month) |>
  summarise(tot_ddd = sum(ddd_100bd, na.rm = TRUE))

file.create("C:/Users/dtra5939/OneDrive - The University of Sydney (Staff)/Documents/03 - Analysis files/01 - AMS/02 - AMS dashboards/Quarto outputs/Sample_Tan Binh dashboard.qmd")

sample |> filter(ingredient %in% c("Levofloxacin", "Ceftriaxon", "Amikacin", "Metronidazol")) |>
  group_by(ingredient, report_month) |>
  summarise(tot_ddd = sum(ddd_100bd, na.rm = TRUE))

str(sample)
