---
title: "Grid Modernisation: Take-or-Pay vs Loose Contract Cost Allocation Model"
author: "Moses Ayirebi"
date: "`r Sys.Date()`"
output:
  pdf_document:
    toc: true
  html_document:
    toc: true
    toc_float: true
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
```

## Overview

This report models how unrecovered grid-capacity cost is allocated between a
data center and residential ratepayers sharing the same local feeder, under
two billing policies: a loosely negotiated contract versus a strict,
Ohio-style take-or-pay tariff (data centers must pay for at least 85% of
their subscribed capacity, regardless of actual usage, for up to 12 years).

## Build the Grid Topology

```{r topology}
library(igraph)
library(tidyverse)
library(lpSolve)

grid_net <- make_empty_graph(directed = FALSE)
grid_net <- add_vertices(grid_net, 3,
                         name = c("Gen_Bus", "Residential_Bus", "DataCenter_Bus"),
                         bus_type = c("Generator", "Residential", "DataCenter"),
                         base_kv = c(230, 230, 230))
grid_net <- add_edges(grid_net, c("Gen_Bus", "Residential_Bus",
                                  "Residential_Bus", "DataCenter_Bus"),
                      reactance = c(0.05, 0.10),
                      thermal_limit_mw = c(800, 520))

LINE1_LIMIT <- E(grid_net)[1]$thermal_limit_mw
LINE2_LIMIT <- E(grid_net)[2]$thermal_limit_mw
```

## Demand and Supply Assumptions

```{r demand}
hours <- 1:8
demand_data <- bind_rows(
  tibble(hour = hours, bus = "Residential_Bus",
         demand_mw = c(100, 90, 80, 85, 110, 150, 200, 180)),
  tibble(hour = hours, bus = "DataCenter_Bus",
         demand_mw = rep(400, 8))
)

knitr::kable(demand_data, caption = "Hourly demand by bus (MW)")

GEN_COST <- 30
VOLL_RES <- 10000
VOLL_DC <- 500
DC_SUBSCRIBED_MW <- 400
TAKE_OR_PAY_PCT <- 0.85
```

## Solve the Optimal Power Flow (OPF), With Curtailment

```{r solver}
solve_hourly_opf <- function(d_res, d_dc) {
  obj_coeffs <- c(GEN_COST, 0, 0, VOLL_RES, VOLL_DC)

  constr_matrix <- matrix(c(
    1, -1,  0,  0,  0,
    0,  1, -1,  0,  0,
    0,  0,  1,  1,  1
  ), nrow = 3, byrow = TRUE)
  rhs_vector <- c(0, 0, d_res + d_dc)
  direction_vector <- c("=", "=", "=")

  constr_matrix <- rbind(constr_matrix,
                         c(0, 1, 0, 0, 0),
                         c(0, 0, 1, 0, 0),
                         c(0, 0, 0, 1, 0),
                         c(0, 0, 0, 0, 1))
  rhs_vector <- c(rhs_vector, LINE1_LIMIT, LINE2_LIMIT, d_res, d_dc)
  direction_vector <- c(direction_vector, "<=", "<=", "<=", "<=")

  opt_result <- lp("min", obj_coeffs, constr_matrix, direction_vector, rhs_vector,
                   compute.sens = TRUE)

  solution <- opt_result$solution
  names(solution) <- c("gen", "flow1", "flow2", "curtail_res", "curtail_dc")

  served_res <- d_res - solution["curtail_res"]
  served_dc  <- d_dc  - solution["curtail_dc"]

  congestion_price <- -opt_result$duals[5]  # lpSolve returns this negative for a
                                             # binding <= constraint; flip the sign
                                             # so congestion raises price, not lowers it
  lmp_shared <- GEN_COST + congestion_price

  tibble(
    served_res_mw = served_res,
    served_dc_mw = served_dc,
    curtailed_dc_mw = solution["curtail_dc"],
    curtailed_res_mw = solution["curtail_res"],
    lmp_shared = lmp_shared
  )
}

physical_results <- map2_df(
  demand_data %>% filter(bus == "Residential_Bus") %>% pull(demand_mw),
  demand_data %>% filter(bus == "DataCenter_Bus") %>% pull(demand_mw),
  ~solve_hourly_opf(.x, .y)
) %>%
  mutate(hour = hours)
```

## Apply the Two Billing Policies

```{r billing}
billing_results <- physical_results %>%
  mutate(
    billed_loose = served_dc_mw,
    billed_take_or_pay = pmax(served_dc_mw, TAKE_OR_PAY_PCT * DC_SUBSCRIBED_MW)
  ) %>%
  pivot_longer(cols = c(billed_loose, billed_take_or_pay),
               names_to = "scenario", values_to = "billed_dc_mw") %>%
  mutate(
    scenario = recode(scenario,
                      billed_loose = "Loose Contract (Cost Leakage)",
                      billed_take_or_pay = "Strict Take-or-Pay (Ohio Model)"),
    revenue_recovered = billed_dc_mw * lmp_shared,
    cost_of_reserved_capacity = DC_SUBSCRIBED_MW * lmp_shared,
    cost_leakage = cost_of_reserved_capacity - revenue_recovered
  )

knitr::kable(
  billing_results %>%
    select(hour, scenario, served_dc_mw, lmp_shared, billed_dc_mw, cost_leakage),
  caption = "Hourly results: physical dispatch is identical across scenarios (same congestion, same price); the policies diverge only in cost recovery",
  digits = 0
)
```

## Total 8-Hour Cost Leakage by Scenario

```{r summary-table}
leakage_summary <- billing_results %>%
  group_by(scenario) %>%
  summarise(total_leakage = sum(cost_leakage))

knitr::kable(leakage_summary, caption = "Total unrecovered capacity cost over the 8-hour window", digits = 0)
```

## Where the Two Policies Actually Diverge

Physical price is driven by congestion, not billing contracts, so the LMP is
identical in both scenarios every hour. The economically meaningful
comparison is cost recovery: a take-or-pay floor recovers more of the
reserved-capacity cost during real congestion, leaving less to be socialized
onto other ratepayers.

```{r plot, fig.width=9, fig.height=5}
leakage_plot <- ggplot(billing_results, aes(x = hour, y = cost_leakage, fill = scenario)) +
  geom_col(position = "dodge") +
  labs(title = "Grid Modernisation: Unrecovered Capacity Cost by Hour",
       subtitle = "Congestion hours (6-8) are identical in price; they diverge in who pays",
       x = "Hour of Day", y = "Unrecovered capacity cost ($), i.e. leakage to other ratepayers") +
  theme_minimal()

print(leakage_plot)

ggsave("leakage_plot_output.png", plot = leakage_plot, width = 9, height = 5, dpi = 150)
```

## Key Takeaway

At hour 7, the worst congestion hour, curtailment pushes the data center's
actual delivered power (320 MW) below the take-or-pay floor (340 MW, 85% of
400 MW subscribed). A loose contract bills only for what was delivered
(320 MW), leaving $40,000 unrecovered that hour. The strict take-or-pay
tariff still bills the 340 MW floor, reducing that hour's leakage to
$30,000, a real $10,000 difference recovered from the data center instead of
being passed on to residential customers.

**Note:** this model uses illustrative, assumed inputs (a 400 MW load, a
$30/MWh generation cost, a 520 MW shared feeder limit), not calibrated data
from a specific utility. It demonstrates that the take-or-pay cost-recovery
mechanism is real and quantifiable; a follow-up step would replace these
inputs with real EIA-861 sales data, an actual wholesale price benchmark,
and a specific utility's tariff terms.
