#Plots----

##All hospitals----

###Line chart comparing 3 hospitals----
tot_ddd_line_all <- sample |>
  group_by(hospital, report_month) |>
  summarise(tot_ddd = sum(ddd_100bd, na.rm = TRUE)) |>
  ggplot(aes(x = report_month, y = tot_ddd)) +
  geom_line(aes(colour = hospital), linemitre = 4) +
  geom_point(size = 4, colour = "steelblue") +
  geom_text(aes(label = tot_ddd), vjust = 1.5, size = 5) +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month") +
  scale_y_continuous(limits = c(0, 48), breaks = seq(0, 48, 4)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_blank()
  ) +
  labs(x = NULL, y = "Total DDD/100 bed-days")

###Top ten AMs consumed by hospitals----

####Hospital A----
top10_a <- sample |>
  filter(hospital == "Hospital A") |>
  group_by(ingredient) |>
  summarise(tot_ddd = sum(ddd_100bd, na.rm = TRUE)) |>
  ggplot(aes(x = reorder(ingredient, tot_ddd), y = tot_ddd)) +
  geom_col(fill = "#384860") +
  geom_text(aes(label = tot_ddd), hjust = -0.1, size = 4) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_blank()
  )

####Hospital B----
top10_b <- sample |>
  filter(hospital == "Hospital B") |>
  group_by(ingredient) |>
  summarise(tot_ddd = sum(ddd_100bd, na.rm = TRUE)) |>
  ggplot(aes(x = reorder(ingredient, tot_ddd), y = tot_ddd)) +
  geom_col(fill = "#384860") +
  geom_text(aes(label = tot_ddd), hjust = -0.1, size = 4) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_blank()
  )

####Hospital C----
top10_c <- sample |>
  filter(hospital == "Hospital C") |>
  group_by(ingredient) |>
  summarise(tot_ddd = sum(ddd_100bd, na.rm = TRUE)) |>
  ggplot(aes(x = reorder(ingredient, tot_ddd), y = tot_ddd)) +
  geom_col(fill = "#384860") +
  geom_text(aes(label = tot_ddd), hjust = -0.1, size = 4) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_blank()
  )

##Hospital A----

###Total DDD/100 bed-days by months----
tot_ddd_line_a <- sample |>
  filter(hospital == "Hospital A") |>
  group_by(report_month) |>
  summarise(tot_ddd = sum(ddd_100bd, na.rm = TRUE)) |>
  ggplot(aes(x = report_month, y = tot_ddd)) +
  geom_line(colour = "tomato") +
  geom_point(colour = "steelblue", size = 5) +
  geom_text(aes(label = tot_ddd), vjust = 1.3, size = 5) +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month") +
  scale_y_continuous(limits = c(0, 19), breaks = seq(0, 19, 2)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold")
  ) +
  labs(x = NULL, y = "Total DDD/100 bed-days")

###Ingredient table----
am_tab_a <- sample |>
  filter(hospital == "Hospital A") |>
  filter(month(report_month) == 4) |>
  group_by(ingredient, report_month) |>
  summarise(tot_ddd = sum(ddd_100bd, na.rm = TRUE)) |>
  ungroup() |>
  select(ingredient, tot_ddd) |>
  arrange(desc(tot_ddd)) |>
  rename("Total DDD/100 bed-days" = tot_ddd, "Ingredients" = ingredient)

###By selected AMs and grouped by month----
am_ddd_bar_a <- sample |>
  filter(hospital == "Hospital A") |>
  filter(
    ingredient %in% c("Levofloxacin", "Ceftriaxon", "Amikacin", "Metronidazol")
  ) |>
  group_by(ingredient, report_month) |>
  summarise(tot_ddd = sum(ddd_100bd, na.rm = TRUE)) |>
  ggplot(aes(ingredient, tot_ddd, fill = as.factor(month(report_month)))) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = tot_ddd),
    position = position_dodge(width = 0.8),
    vjust = -0.5,
    size = 4
  ) +
  scale_fill_brewer(
    palette = "Oranges",
    labels = c("Feb - 2026", "Mar - 2026", "Apr - 2026")
  ) +
  scale_y_continuous(limits = c(0, 8), breaks = seq(0, 8, 0.5)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 16, face = "bold")
  ) +
  labs(y = "DDD/100 bed-days")

##Hospital B----

###Total DDD/100 bed-days by months----
tot_ddd_line_b <- sample |>
  filter(hospital == "Hospital B") |>
  group_by(report_month) |>
  summarise(tot_ddd = sum(ddd_100bd, na.rm = TRUE)) |>
  ggplot(aes(x = report_month, y = tot_ddd)) +
  geom_line(colour = "tomato") +
  geom_point(colour = "steelblue", size = 5) +
  geom_text(aes(label = tot_ddd), vjust = 1.3, size = 5) +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month") +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold")
  ) +
  labs(x = NULL, y = "Total DDD/100 bed-days")

###Ingredient table----
am_tab_b <- sample |>
  filter(hospital == "Hospital B") |>
  filter(month(report_month) == 4) |>
  group_by(ingredient, report_month) |>
  summarise(tot_ddd = sum(ddd_100bd, na.rm = TRUE)) |>
  ungroup() |>
  select(ingredient, tot_ddd) |>
  arrange(desc(tot_ddd)) |>
  rename("Total DDD/100 bed-days" = tot_ddd, "Ingredients" = ingredient)

###By selected AMs and grouped by month----
am_ddd_bar_b <- sample |>
  filter(hospital == "Hospital B") |>
  filter(
    ingredient %in% c("Levofloxacin", "Ceftriaxon", "Amikacin", "Metronidazol")
  ) |>
  group_by(ingredient, report_month) |>
  summarise(tot_ddd = sum(ddd_100bd, na.rm = TRUE)) |>
  ggplot(aes(ingredient, tot_ddd, fill = as.factor(month(report_month)))) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = tot_ddd),
    position = position_dodge(width = 0.8),
    vjust = -0.5,
    size = 4
  ) +
  scale_fill_brewer(
    palette = "Oranges",
    labels = c("Feb - 2026", "Mar - 2026", "Apr - 2026")
  ) +
  scale_y_continuous(limits = c(0, 8), breaks = seq(0, 8, 0.5)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 16, face = "bold")
  ) +
  labs(y = "DDD/100 bed-days")

##Hospital C----

###Total DDD/100 bed-days by months----
tot_ddd_line_c <- sample |>
  filter(hospital == "Hospital C") |>
  group_by(report_month) |>
  summarise(tot_ddd = sum(ddd_100bd, na.rm = TRUE)) |>
  ggplot(aes(x = report_month, y = tot_ddd)) +
  geom_line(colour = "tomato") +
  geom_point(colour = "steelblue", size = 5) +
  geom_text(aes(label = tot_ddd), vjust = 1.3, size = 5) +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month") +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, 5)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold")
  ) +
  labs(x = NULL, y = "Total DDD/100 bed-days")

###Ingredient table----
am_tab_c <- sample |>
  filter(hospital == "Hospital C") |>
  filter(month(report_month) == 4) |>
  group_by(ingredient, report_month) |>
  summarise(tot_ddd = sum(ddd_100bd, na.rm = TRUE)) |>
  ungroup() |>
  select(ingredient, tot_ddd) |>
  arrange(desc(tot_ddd)) |>
  rename("Total DDD/100 bed-days" = tot_ddd, "Ingredients" = ingredient)

###By selected AMs and grouped by month----
am_ddd_bar_c <- sample |>
  filter(hospital == "Hospital C") |>
  filter(
    ingredient %in% c("Levofloxacin", "Ceftriaxon", "Amikacin", "Metronidazol")
  ) |>
  group_by(ingredient, report_month) |>
  summarise(tot_ddd = sum(ddd_100bd, na.rm = TRUE)) |>
  ggplot(aes(ingredient, tot_ddd, fill = as.factor(month(report_month)))) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = tot_ddd),
    position = position_dodge(width = 0.8),
    vjust = -0.5,
    size = 4
  ) +
  scale_fill_brewer(
    palette = "Oranges",
    labels = c("Feb - 2026", "Mar - 2026", "Apr - 2026")
  ) +
  scale_y_continuous(limits = c(0, 8), breaks = seq(0, 8, 0.5)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 16, face = "bold")
  ) +
  labs(y = "DDD/100 bed-days")
