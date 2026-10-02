# LOAD PACKAGES ----

# Packages for data editing
librarian::shelf(tidyverse, eurostat, readxl, haven, car, DIGCLASS)
# Packages for tables
librarian::shelf(texreg, gt, gtExtras, Hmisc)
# Packages for figures
librarian::shelf(ggradar, ggrepel)


# LOAD AND PREPARE DATA ----

## Load ESS data ----
# Please download ESS Round 9 (Ed. 3.1) for free at https://ess.sikt.no/en/datafile/b2b0bf39-176b-4eca-8d26-3c05ea83d2cb after registering. The file format is Stata *.dta!
ess <- read_dta("ESS9e03_1.dta", encoding = "latin1")

# Convert haven_labelled into numeric
is.havenlab <- function(x) "haven_labelled" %in% class(x)
ess <- ess |> mutate(across(where(is.havenlab), as.numeric))

## Create new variables ----
ess <- ess |> 
  mutate(dpweight = pspwght*pweight,
         d_wltdffr = recode_values(wltdffr, 1:4 ~ 1, 0:-4 ~ 0),
         female = recode_values(gndr, 2 ~ 1, 1 ~ 0),
         academic = recode_values(eisced, 6:7 ~1, c(0:5, 55) ~ 0),
         brncntry = recode_values(brncntr, 1 ~ 0, 2 ~ 1),
         d_topinfr = recode_values(topinfr, 1:4 ~ 1, 0:-4 ~ 0),
         r_sofrdst = car::recode(sofrdst, "1=5;2=4;4=2;5=1"),
         r_sofrwrk = car::recode(sofrwrk, "1=5;2=4;4=2;5=1"),
         r_sofrpr = car::recode(sofrpr, "1=5;2=4;4=2;5=1"),
         r_sofrprv = car::recode(sofrprv, "1=5;2=4;4=2;5=1"),
         d_sofrdst = recode_values(sofrdst, 1:2 ~ 1, 3:5 ~ 0),
         d_sofrwrk = recode_values(sofrwrk, 1:2 ~ 1, 3:5 ~ 0),
         d_sofrpr = recode_values(sofrpr, 1:2 ~ 1, 3:5 ~ 0),
         d_sofrprv = recode_values(sofrprv, 1:2 ~ 1, 3:5 ~ 0),
         selfemployed = case_when(emplrel == 2 ~ 1, emplrel %in% c(1,3) ~ 0, TRUE ~ NA),
         supervisor = recode_values(jbspv, 1 ~ 1, 2 ~ 0),
         control = recode_values(wkdcorga, 8:10 ~ 1, 5:7 ~ 2, 2:4 ~ 3, 0:1 ~4),
         isco88 = isco08_to_isco88(isco08),
         isco88com = isco88_to_isco88com(isco88),
         wright = factor(isco88com_to_wright(x = isco88com, is_supervisor = supervisor, self_employed = selfemployed, n_employees = emplno, control_work = iorgact, control_daily = control, type = "simple", label = T, to_factor = T), ordered = F),
         oesch5 = isco08_to_oesch(x = isco08, self_employed = selfemployed, 
                                  n_employees = emplno, n_classes = 5),
         oesch16 = isco08_to_oesch(x = isco08, self_employed = selfemployed, 
                                   n_employees = emplno, n_classes = 16),
         oesch6 = ifelse(oesch16 == 1, 0, as.numeric(oesch5)),
         oesch = fct_relevel(recode_values(oesch6, 
                    0 ~ "Large employers",
                    1 ~ "Higher-grade service class",
                    2 ~ "Lower-grade service class",
                    3 ~ "Small business owners",
                    4 ~ "Skilled workers",
                    5 ~ "Unskilled workers"),
                    "Large employers",
                    "Small business owners",
                    "Higher-grade service class",
                    "Lower-grade service class",
                    "Skilled workers",
                    "Unskilled workers"))


## Specify welfare regimes ----
cntry <- tibble(cntry = unique(ess$cntry), regime = NA) |> 
  mutate(regime = recode_values(cntry,
                             c("AT","BE","DE","FR","NL") ~ "Conservative",
                             c("FI","NO","SE","DK","IS") ~ "Social democratic",
                             c("BG","CZ","EE","HR","HU","LT","LV","ME","PL","RS","SI","SK") ~ "Post-socialist",
                             c("IE","GB","CH") ~ "Liberal",
                             c("CY","ES","IT","PT") ~ "Family-oriented"))


## Load wealth inequality data ----
widdat <- readRDS("data/wid.RData") |> 
  filter(year == 2018) |> 
  select(cntry, wlth_top5)


## Merge country table with inequality information ----
cntry <- left_join(cntry, widdat) |> 
  mutate(wigroup = case_when(
    wlth_top5 < 40 ~ "low", 
    wlth_top5 > 50 ~ "high", 
    TRUE ~ "medium"),
        wigroup = factor(wigroup, levels = c("high", "medium", "low")),
        wigroup_t = factor(as.numeric(cut_number(wlth_top5, 3))),
        wigroup_t = fct_recode(wigroup_t, low = "1", medium = "2", high = "3")) 


## Select regression variables ----
vars <- tribble(~variable, ~name, 
                "dpweight", "Weights",
                "cntry", "Country",
                "wltdffr", "Perception of wealth differences",
                "oesch", "Social class",
                "oeschregLarge employers", "Large employers",
                "oeschregSmall business owners", "Small business owners",
                "oeschregHigher-grade service class", "Higher-grade service class",
                "oeschregLower-grade service class", "Lower-grade service class",
                "oeschregSkilled workers", "Skilled workers",
                "female", "Gender (female)",
                "agea", "Age",
                "academic", "Tertiary edu.",
                "brncntry", "Born abroad",
                "r_sofrdst", "Justice principle: Equality",
                "r_sofrwrk", "Justice principle: Equity",
                "r_sofrpr", "Justice principle: Need",
                "r_sofrprv", "Justice principle: Entitlement",
                "d_sofrdst", "JP: Equality",
                "d_sofrwrk", "JP: Equity",
                "d_sofrpr", "JP: Need",
                "d_sofrprv", "JP: Entitlement",
                "d_topinfr", "Top income perceived unfair",
                "(Intercept)", "Constant") |> 
  mutate(variable = fct_inorder(variable))

## Create regression dataset ----
regdat <- ess |>
  select(any_of(vars$variable)) |> 
  left_join(cntry, by = "cntry") |> 
  fastDummies::dummy_cols(select_columns = "cntry", remove_most_frequent_dummy = T) |>
  mutate(wltdffrord = factor(wltdffr, ordered = T, levels = -4:4),
        oeschreg = relevel(oesch, ref = "Unskilled workers"),
         across(starts_with("r_"), ~factor(., ordered = T, levels = 1:5))) |> 
  drop_na()



# FIGURES ----

## Figure 1: Perceptions of unfairness of income and wealth inequalities ----
esswltinc <- ess |> 
  select(cntry, dweight, pweight, d_wltdffr, d_topinfr) |> 
  pivot_longer(cols = c(d_wltdffr, d_topinfr), names_to = "var", values_to = "value") |> 
  filter(!is.na(value)) |> 
  summarise(anteil = sum(value*dweight)/sum(dweight), 
            weight = mean(pweight), 
            .by = c(cntry, var)) |> 
  left_join(cntry |> select(cntry, regime))

orderbywealth <- esswltinc |> 
  filter(var == "d_wltdffr") |> 
  arrange(-anteil) |> 
  pull(cntry)

plotdat <- esswltinc |> 
  mutate(cntry = factor(cntry, levels = orderbywealth), 
        anteil = anteil*100)

plotdat |> 
  ggplot(aes(x = cntry, y = anteil, group = var)) +
  geom_linerange(data = plotdat |> group_by(cntry) |> slice_max(anteil,n=1), 
                 aes(ymin = 0, ymax = anteil), linewidth = 5.3, 
                 alpha = .3, color = "gray80") +
  geom_point(aes(color = var), size = 5, shape = 15) + 
  geom_text(aes(label = round(anteil, 0)), color = "white", size = 3.3, hjust = .5,
            vjust = .5, family = "Roboto Condensed") +
  scale_color_manual(name = NULL,
                     values = c("gray20", "gray60"), 
                     labels = c("Income of top 10%", "Wealth differences"),
                     guide = guide_legend(reverse = T)) +
  scale_y_continuous(labels = scales::percent_format(scale=1)) + 
  labs(x = NULL, y = "Share of response: unfairly high") +
  theme_minimal(base_family = "Roboto Condensed", base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.25),
        panel.grid.major.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = "inside",
        legend.position.inside = c(0.85, 0.85),
        legend.title = element_text(size = 11, margin = margin(r = 3)),
        legend.text = element_text(size = 11),
        legend.key.spacing = unit(0, "pt"),
        legend.key.spacing.x = unit(0.5, "lines"))

ggsave("figures/Fig_1.png", width = 8, height = 4, dpi = 320, bg = "white")
ggsave("figures/Fig_1.pdf", width = 8, height = 4, device = cairo_pdf)


## Figure 2: Perceptions of wealth inequality by social class ----
regdat |> 
  count(oesch, wltdffr, wt = dpweight) |> 
  drop_na() |> 
  mutate(prop = prop.table(n), .by = oesch) |> 
  ggplot(aes(x = as.factor(wltdffr), y = oesch, fill = prop)) +
  geom_tile() +
  geom_text(aes(label = round(prop*100, 1)), color = "white",
            family = "Barlow Condensed") +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_family = "Barlow Condensed", base_size = 14) +
  theme(legend.position = "none",
        panel.grid = element_blank())

ggsave("figures/Fig_2.png", width = 7, height = 5, dpi = 320, bg = "white")
ggsave("figures/Fig_2.pdf", width = 7, height = 5, device = cairo_pdf)


## Figure 3: Social justice principles by social class ----
regdat |> 
  select(oesch, d_sofrdst, d_sofrwrk, d_sofrpr, d_sofrprv, dpweight) |> 
  pivot_longer(starts_with("d_"), names_to = "var", values_to = "values") |> 
  count(oesch, var, values, wt = dpweight) |> 
  drop_na() |> 
  mutate(prop = prop.table(n), .by = c(oesch, var)) |> 
  filter(values == 1) |> 
  select(var, oesch, prop) |> 
  mutate(var = case_match(var, 
        "d_sofrdst" ~ "JP: Equality",
        "d_sofrwrk" ~ "JP: Equity",
        "d_sofrpr" ~ "JP: Need",
        "d_sofrprv" ~ "JP: Entitlement"),
        oesch = str_wrap(oesch, 15)) |>    
  pivot_wider(names_from = "oesch", values_from = "prop") |>  
  ggradar(font.radar = "Barlow Condensed",
        legend.title = "",
        legend.position = "bottom",
        values.radar = c("", "50%", "100%"),
        axis.label.size = 4,
        grid.label.size = 5, 
        grid.line.width = 0.3,
        gridline.mid.colour = "grey",
        legend.text.size = 10,
        group.line.width = 1,
        group.point.size = 3,
        group.colours = c("#f1af3a", "#cf5e4e", "#637b31", "#003967"))

ggsave("figures/Fig_3.png", width = 7, height = 5, dpi = 320, bg = "white")
ggsave("figures/Fig_3.pdf", width = 7, height = 5, device = cairo_pdf)


## Figure 4: Mean perceptions of wealth inequality by social class across countries with low, medium, and high wealth inequality ----
mwi <- regdat |> 
  summarise(n = n(),
            mperc = wtd.mean(wltdffr, weights = dpweight, na.rm = T),
            var = wtd.var(wltdffr, weights = dpweight, na.rm =T),
            sd = sqrt(var),
            .by = c(oesch, wigroup_t))

mwi |> 
  ggplot(aes(y = wigroup_t, color = oesch)) +
  geom_point(aes(x = mperc), size = 4, shape = 19, alpha = 0.6) +
  geom_text(aes(x = mperc, label = scales::number_format(accuracy = .1)(mperc)), 
  nudge_x = -0.15, family = "Barlow Condensed", size = 3.8, hjust = 1, show.legend = F,
  data = mwi |> slice_min(mperc, by = wigroup_t)) +
  geom_text(aes(x = mperc, label = scales::number_format(accuracy = .1)(mperc)), 
  nudge_x = 0.15, family = "Barlow Condensed", size = 3.8, hjust = 0, show.legend = F,
  data = mwi |> slice_max(mperc, by = wigroup_t)) +
  scale_x_continuous(limits = c(-4,4), expand = c(0,0)) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_family = "Barlow Condensed", base_size = 14) +
  theme(legend.position = "bottom",
  panel.grid.major.y = element_blank(),
  panel.grid.minor = element_blank(),
  legend.title = element_blank())

ggsave("figures/Fig_4.png", width = 7, height = 4, dpi = 320, bg = "white")
ggsave("figures/Fig_4.pdf", width = 7, height = 4, device = cairo_pdf)


## Figure 5: Percentage viewing wealth inequality as "unfairly large" by social class across countries with low, medium, and high wealth inequality ----
regdat |> 
  mutate(d_wltdffr = case_match(wltdffr, 1:4 ~ 1, 0:-4 ~ 0)) |> 
  count(oesch, d_wltdffr, wigroup_t, wt = dpweight) |> 
  mutate(prop = prop.table(n), .by = c(oesch, wigroup_t)) |> 
  filter(d_wltdffr == 1) |> 
  ggplot(aes(x = prop, y = wigroup_t, color = oesch, fill = oesch)) +
  geom_segment(aes(xend = 0, yend = wigroup_t), position = position_dodge(width = 0.8, reverse = T)) + 
  geom_point(position = position_dodge(width = 0.8, reverse = T)) + 
  scale_x_continuous(labels = scales::percent_format(scale = 100)) +
  labs(x = "Differences in wealth are unfairly high", y = NULL) +
  theme_minimal(base_family = "Barlow Condensed", base_size = 14) +
  theme(legend.position = "bottom",
  panel.grid.major.y = element_blank(),
  panel.grid.minor = element_blank(),
  legend.title = element_blank())

ggsave("figures/Fig_5.png", width = 7, height = 5, dpi = 320, bg = "white")
ggsave("figures/Fig_5.pdf", width = 7, height = 5, device = cairo_pdf)


## Figure 6: Mean perceptions of wealth inequality by social class across welfare state regimes ----
mwr <- regdat |> 
  summarise(n = n(), 
            mperc = wtd.mean(wltdffr, weights = dpweight, na.rm = T), 
            var = wtd.var(wltdffr, weights = dpweight, na.rm =T),
            sd = sqrt(var),
            .by = c(oesch, regime))

mwr |>
  ggplot(aes(y = regime, color = oesch)) +
  geom_point(aes(x = mperc), size = 4, shape = 19, alpha = 0.6) +
  geom_text(aes(x = mperc, label = scales::number_format(accuracy = .1)(mperc)), 
  nudge_x = -0.15, family = "Barlow Condensed", size = 3.8, hjust = 1, show.legend = F,
  data = mwr |> slice_min(mperc, by = regime)) +
  geom_text(aes(x = mperc, label = scales::number_format(accuracy = .1)(mperc)), 
  nudge_x = 0.15, family = "Barlow Condensed", size = 3.8, hjust = 0, show.legend = F,
  data = mwr |> slice_max(mperc, by = regime)) +
  scale_x_continuous(limits = c(-4,4), expand = c(0,0)) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_family = "Barlow Condensed", base_size = 14) +
  theme(legend.position = "bottom",
  panel.grid.major.y = element_blank(),
  panel.grid.minor = element_blank(),
  legend.title = element_blank())

ggsave("figures/Fig_6.png", width = 7, height = 4, dpi = 320, bg = "white")
ggsave("figures/Fig_6.pdf", width = 7, height = 4, device = cairo_pdf)


## Figure 7: Percentage viewing wealth inequality as "unfairly large" by social class across welfare state regimes ----
regdat |> 
  mutate(d_wltdffr = case_match(wltdffr, 1:4 ~ 1, 0:-4 ~ 0)) |> 
  count(oesch, d_wltdffr, regime, wt = dpweight) |> 
  mutate(prop = prop.table(n), .by = c(oesch, regime)) |> 
  filter(d_wltdffr == 1) |> 
  ggplot(aes(x = prop, y = regime, color = oesch, fill = oesch)) +
  geom_segment(aes(xend = 0, yend = regime), position = position_dodge(width = 0.8, reverse = T)) + 
  geom_point(position = position_dodge(width = 0.8, reverse = T)) + 
  scale_x_continuous(labels = scales::percent_format(scale = 100)) +
  labs(x = "Differences in wealth are unfairly high", y = NULL) +
  theme_minimal(base_family = "Barlow Condensed", base_size = 14) +
  theme(legend.position = "bottom",
  panel.grid.major.y = element_blank(),
  panel.grid.minor = element_blank(),
  legend.title = element_blank())

ggsave("figures/Fig_7.png", width = 7, height = 5, dpi = 320, bg = "white")
ggsave("figures/Fig_7.pdf", width = 7, height = 5, device = cairo_pdf)


## Figure A1: Perceptions of wealth inequality by social class (Wright) ----
ess |> count(wright, wltdffr, wt = dpweight) |> 
  drop_na() |> 
  mutate(prop = prop.table(n), .by = wright) |> 
  ggplot(aes(x = as.factor(wltdffr), y = wright, fill = prop)) +
  geom_tile() +
  geom_text(aes(label = round(prop*100, 1)), color = "white",
            family = "Barlow Condensed") +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_family = "Barlow Condensed", base_size = 14) +
  theme(legend.position = "none")

ggsave("figures/Fig_A1.png", width = 7, height = 5, dpi = 320, bg = "white")
ggsave("figures/Fig_A1.pdf", width = 7, height = 5, device = cairo_pdf)


## Figure A2: Social justice principles by social class (Wright) ----
ess |> 
  select(wright, d_sofrdst, d_sofrwrk, d_sofrpr, d_sofrprv, dpweight) |> 
  pivot_longer(starts_with("d_"), names_to = "var", values_to = "values") |> 
  count(wright, var, values, wt = dpweight) |> 
  drop_na() |> 
  mutate(prop = prop.table(n), .by = c(wright, var)) |> 
  filter(values == 1) |> 
  select(var, wright, prop) |> 
  mutate(var = case_match(var, 
        "d_sofrdst" ~ "JP: Equality",
        "d_sofrwrk" ~ "JP: Equity",
        "d_sofrpr" ~ "JP: Need",
        "d_sofrprv" ~ "JP: Entitlement"),
        wright = str_wrap(wright, 15)) |>    
  pivot_wider(names_from = "wright", values_from = "prop") |>  
  ggradar(font.radar = "Barlow Condensed",
        legend.title = "",
        legend.position = "bottom",
        values.radar = c("", "50%", "100%"),
        axis.label.size = 4,
        grid.label.size = 5, 
        grid.line.width = 0.3,
        gridline.mid.colour = "grey",
        legend.text.size = 10,
        group.line.width = 1,
        group.point.size = 3,
        group.colours = c("#f1af3a", "#cf5e4e", "#637b31", "#003967"))

ggsave("figures/Fig_A2.png", width = 7, height = 5, dpi = 320, bg = "white")
ggsave("figures/Fig_A2.pdf", width = 7, height = 5, device = cairo_pdf)


# TABLES ----

## Table 2: Distribution of observations across Oesch classes ----
regdat |> 
  count(oesch) |> 
  mutate(freq = n/sum(n)*100) |>
  janitor::adorn_totals("row") |> 
  gt() |> 
  cols_label(oesch ~ "Oesch class", n ~ "Number of observations", freq ~ "Share of sample") |> 
  cols_align(align = "left", columns = oesch) |>
  fmt_integer(columns = "n") |> 
  fmt_percent(columns = "freq", decimals = 1, scale_values = F) |>
  gtsave("tables/Tab_2.html")


## Table 4: Effect of social classes and social justice principles on the perception of wealth inequalities (ordered logistic regression models) ----
olfe <- ordinal::clm(wltdffrord ~ oeschreg + female + agea + academic + brncntry, 
                     data = regdat, weights = dpweight)
olfe1 <- ordinal::clm(wltdffrord ~ oeschreg + female + agea + academic + brncntry + d_sofrdst + d_sofrwrk + d_sofrpr + d_sofrprv, 
                     data = regdat, weights = dpweight)
olfe2 <- ordinal::clm(r_sofrdst ~ oeschreg + female + agea + academic + brncntry, 
                     data = regdat, weights = dpweight)
olfe3 <- ordinal::clm(r_sofrwrk ~ oeschreg + female + agea + academic + brncntry, 
                     data = regdat, weights = dpweight)
olfe4 <- ordinal::clm(r_sofrpr ~ oeschreg + female + agea + academic + brncntry, 
                     data = regdat, weights = dpweight)
olfe5 <- ordinal::clm(r_sofrprv ~ oeschreg + female + agea + academic + brncntry, 
                     data = regdat, weights = dpweight)

htmlreg(list(olfe, olfe1, olfe2, olfe3, olfe4, olfe5), file = "tables/Tab_4.html",
        custom.coef.map = split(vars$name, vars$variable),
        include.nobs = FALSE,
        custom.gof.rows = list("Observations" = rep(nrow(regdat), 6)),
        digits = 3, stars = c(0.001, 0.01, 0.05),
        caption = NULL,
        custom.model.names = c("Inequality", "Inequality","Equality", "Equity", "Need", "Status"))


## Table A1: Classification of countries by the extent of wealth inequality ----
regdat |> 
  count(cntry) |>
  left_join(cntry |> 
  select(cntry, wlth_top5, wigroup_t)) |> 
  arrange(wlth_top5) |> 
  gt() |> 
  cols_align(align = "right", columns = wigroup_t) |>
  fmt_integer(columns = "n") |> 
  fmt_percent(columns = "wlth_top5", decimals = 1, scale_values = F) |>
  cols_label(cntry ~ "Country", n ~ "Obs.", wlth_top5 ~ "Top 5% share", wigroup_t ~ "Group") |> 
  gtsave("tables/Tab_A1.html")



## Table A2: Effect of social classes (Wright) and social justice principles on the perception of wealth inequalities (ordered logistic regression models) ----
vars_wr <- tribble(~variable, ~name, 
                "dpweight", "Weights",
                "cntry", "Country",
                "wltdffr", "Perception of wealth differences",
                "wright", "Social class",
                "wrightSelf empl w/10+ employees", "Large employers",
                "wrightSelf empl w/1-9 employees", "Small business owners",
                "wrightSelf empl w/no empoyees", "Self-employed",
                "wrightExpert managers", "Expert managers",
                "wrightExpert workers", "Expert workers",
                "wrightSkilled manager/superv", "Skilled managers",
                "wrightSkilled workers", "Skilled workers",
                "wrightLow skilled manager/superv", "Low skilled managers",
                "female", "Gender (female)",
                "agea", "Age",
                "academic", "Tertiary edu.",
                "brncntry", "Born abroad",
                "r_sofrdst", "Justice principle: Equality",
                "r_sofrwrk", "Justice principle: Equity",
                "r_sofrpr", "Justice principle: Need",
                "r_sofrprv", "Justice principle: Entitlement",
                "d_sofrdst", "JP: Equality",
                "d_sofrwrk", "JP: Equity",
                "d_sofrpr", "JP: Need",
                "d_sofrprv", "JP: Entitlement",
                "d_topinfr", "Top income perceived unfair",
                "(Intercept)", "Constant") |> 
  mutate(variable = fct_inorder(variable))

regdat_wr <- ess |>
  select(any_of(vars_wr$variable)) |> 
  left_join(cntry, by = "cntry") |> 
  fastDummies::dummy_cols(select_columns = "cntry", remove_most_frequent_dummy = T) |>
  mutate(wltdffrord = factor(wltdffr, ordered = T, levels = -4:4),
         wright = relevel(wright, ref = "Low skilled workers"),
         across(starts_with("r_"), ~factor(., ordered = T, levels = 1:5))) |> 
  drop_na()

olfe <- ordinal::clm(wltdffrord ~ wright + female + agea + academic + brncntry, 
                     data = regdat_wr, weights = dpweight)
olfe1 <- ordinal::clm(wltdffrord ~ wright + female + agea + academic + brncntry + d_sofrdst + d_sofrwrk + d_sofrpr + d_sofrprv, 
                     data = regdat_wr, weights = dpweight)
olfe2 <- ordinal::clm(r_sofrdst ~ wright + female + agea + academic + brncntry, 
                     data = regdat_wr, weights = dpweight)
olfe3 <- ordinal::clm(r_sofrwrk ~ wright + female + agea + academic + brncntry, 
                     data = regdat_wr, weights = dpweight)
olfe4 <- ordinal::clm(r_sofrpr ~ wright + female + agea + academic + brncntry, 
                     data = regdat_wr, weights = dpweight)
olfe5 <- ordinal::clm(r_sofrprv ~ wright + female + agea + academic + brncntry, 
                     data = regdat_wr, weights = dpweight)

htmlreg(list(olfe, olfe1, olfe2, olfe3, olfe4, olfe5), file = "tables/Tab_A2.html",
        custom.coef.map = split(vars_wr$name, vars_wr$variable),
                include.nobs = FALSE,
        custom.gof.rows = list("Observations" = rep(nrow(regdat_wr), 6)),
        digits = 3, stars = c(0.001, 0.01, 0.05),
        caption = NULL,
        custom.model.names = c("Inequality", "Inequality","Equality", "Equity", "Need", "Status"))
