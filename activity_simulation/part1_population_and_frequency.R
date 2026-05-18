# =============================================================================
# SIMBA MOBi Simulation — PART 1 of 5
# Scherr et al. (2020), EJTIR 20(4), pp. 152-172
#
# Covers:
#   1. Synthetic population generation
#   2. Long-term choices (car availability, PT subscription)
#   3. Tour frequency — 4 separate MNL models (Table 1)
#   4. Stop frequency — 4 separate MNL models (Table 1)
#   5. Activity type assignment (segmented by person group)
#
# OUTPUTS: part1_output.RData
#   - pop         : synthetic population data frame (N_AGENTS rows)
#   - agent_tours : list of tour/stop/activity choices per agent
#   - N_AGENTS    : number of agents (passed to next parts)
#
# Run next: part2_destination_mode.R
# =============================================================================

set.seed(42)
library(dplyr)

# =============================================================================
# ASSUMPTIONS AND DEVIATIONS FROM PAPER — emitted as messages at runtime
# =============================================================================
message("[ASSUMPTION 1 – Part 1] Location choice (Section 2.3): The paper uses",
        " a nested MNL with logsum from mode choice, piecewise-linear distance",
        " decay, and shadow prices over 8,000 zones. Here, assign_primary_destination()",
        " assigns locations by UNIFORM RANDOM SAMPLING. No MNL is applied.")
message("[ASSUMPTION 2 – Part 1] Car/PT subscription (Section 2.3): The paper",
        " applies the model by Danalet et al. (2018), with accessibility as an",
        " explanatory variable. Here, HARDCODED PROBABILITIES are used instead.")
message("[ASSUMPTION 3 – Part 1] All MNL beta coefficients in tour/stop frequency",
        " models are INVENTED (directionally plausible) — not estimated from MZ",
        " 2015 data via Biogeme as stated in Section 2.4.")
message("[BUG FIX – Part 1] The subtour activity-type call previously passed",
        " direction='inbound', bypassing the subtour probability table entirely.",
        " Fixed to direction='subtour'; assign_activity_type() restructured so",
        " the subtour branch is reachable before employment-type checks.")
message("[CHANGE 1 – Part 1] Work tours now store subtour_idx: the integer position",
        " of the subtour activity in the activities vector (NA if no subtour).",
        " All non-work tours also carry subtour_idx=NA for structural consistency.",
        " This field is consumed by Part 2 for subtour-specific mode choice.")
message("[FIX DIV-1/7 – Part 1] secondary_stop_freq() rebuilt as ONE single MNL",
        " per Table 1: predicts TOTAL stops (0/1/2) for secondary tours using",
        " variables: constant, employment, age, children_in_HH, car_avail,",
        " PT subscription, n_tours, is_business_tour (binary), acc_home.",
        " The paper does NOT split secondary stops into outbound/inbound;",
        " the previous 8-cell architecture is removed. Helpers",
        " predict_mode_proxy() and activity_to_purpose() also removed.",
        " n_stops=1 -> 1 inbound stop; n_stops=2 -> 1 outbound + 1 inbound.")

N_AGENTS <- 100   # change to any number
N_ZONES <- 20
# =============================================================================
# 1. SYNTHETIC POPULATION
# =============================================================================

generate_population <- function(n) {
  data.frame(
    agent_id       = 1:n,
    age            = sample(18:80, n, replace = TRUE),
    # employment: full_time, part_time_hi (>40%), part_time_lo (<=40%),
    #             not_employed (includes retired, unemployed)
    employment     = sample(
                       c("full_time","part_time_hi","part_time_lo","not_employed"),
                       n, replace = TRUE, prob = c(0.40, 0.20, 0.15, 0.25)),
    in_education   = rbinom(n, 1, 0.15),
    in_management  = rbinom(n, 1, 0.10),
    children_in_hh = rbinom(n, 1, 0.35),
    dist_primary_km       = pmax(1, rnorm(n, mean = 12, sd = 8)),
    accessibility_home    = runif(n, 0.2, 1.0),
    accessibility_primary = runif(n, 0.2, 1.0),
    stringsAsFactors = FALSE
  )
}

pop <- generate_population(N_AGENTS)

# =============================================================================
# 2. LONG-TERM CHOICES: Car availability & PT subscription (Section 2.3)
# =============================================================================

assign_mobility_tools <- function(pop) {
  pop %>% mutate(
    p_car = case_when(
      employment == "full_time"    ~ 0.75,
      employment == "part_time_hi" ~ 0.70,
      employment == "part_time_lo" ~ 0.60,
      TRUE                         ~ 0.45
    ),
    p_car     = ifelse(age > 70, p_car * 0.6, p_car),
    car_avail = rbinom(n(), 1, p_car),
    pt_sub = case_when(
      in_education == 1 ~
        sample(c("none","halbtax","verbund","general"),
               n(), replace = TRUE, prob = c(0.10, 0.20, 0.50, 0.20)),
      employment == "full_time" ~
        sample(c("none","halbtax","verbund","general"),
               n(), replace = TRUE, prob = c(0.30, 0.35, 0.20, 0.15)),
      TRUE ~
        sample(c("none","halbtax","verbund","general"),
               n(), replace = TRUE, prob = c(0.45, 0.30, 0.15, 0.10))
    )
  ) %>% select(-p_car)
}

pop <- assign_mobility_tools(pop)

assign_home_zone <- function(pop, n_zones = N_ZONES) {
  
  pop$home_zone <- sample(1:n_zones, nrow(pop), replace = TRUE)
  
  pop
}

pop <- assign_home_zone(pop)

assign_primary_destination <- function(pop, n_zones = N_ZONES) {
  # [ASSUMPTION 1 – deviation from Section 2.3]: The paper computes location
  # choice probabilities at zone level using a nested MNL incorporating logsum
  # from mode choice, piecewise-linear distance, shadow prices, and zone
  # attraction (number of jobs). Below, zones are assigned by UNIFORM RANDOM
  # SAMPLING with no model structure.
  pop$primary_dest_zone <- NA_integer_
  
  for (i in 1:nrow(pop)) {
    
    if (pop$employment[i] %in% c("full_time","part_time_hi","part_time_lo") ||
        pop$in_education[i] == 1) {
      
      pop$primary_dest_zone[i] <- sample(1:n_zones, 1)
    }
  }
  
  pop
}

pop = assign_primary_destination(pop)

cat("=== Part 1: Population summary ===\n")
cat(sprintf("N agents          : %d\n", nrow(pop)))
cat(sprintf("Car available     : %.1f%%\n", 100 * mean(pop$car_avail)))
cat(sprintf("PT subscription   : %s\n",
    paste(names(table(pop$pt_sub)), table(pop$pt_sub), sep="=", collapse=", ")))
cat(sprintf("Employed          : %.1f%%\n",
    100 * mean(pop$employment != "not_employed")))
cat(sprintf("In education      : %.1f%%\n", 100 * mean(pop$in_education)))
cat("\n")

# =============================================================================
# HELPERS: MNL choice + dummy variable extractors
# Reference categories: employment = "not_employed", PT sub = "none"
# =============================================================================

mnl_choice <- function(utils) {
  exp_u <- exp(utils - max(utils))
  probs <- exp_u / sum(exp_u)
  sample(names(utils), 1, prob = probs)
}

emp_dummies <- function(agent) {
  list(
    d_fulltime    = as.integer(agent$employment == "full_time"),
    d_parttime_hi = as.integer(agent$employment == "part_time_hi"),
    d_parttime_lo = as.integer(agent$employment == "part_time_lo")
  )
}

pt_dummies <- function(agent) {
  list(
    d_halbtax = as.integer(agent$pt_sub == "halbtax"),
    d_verbund  = as.integer(agent$pt_sub == "verbund"),
    d_general  = as.integer(agent$pt_sub == "general")
  )
}

# =============================================================================
# 3. TOUR FREQUENCY — 4 separate MNL models (Table 1, Section 2.4)
#
# IMPORTANT: All beta coefficients are invented — directionally plausible
# but not estimated from data. Variable inclusion follows Table 1 strictly.
# =============================================================================

# --- 3a. Work tour frequency (0/1/2) ---
work_tour_freq <- function(agent) {
  if (!agent$employment %in% c("full_time","part_time_hi","part_time_lo"))
    return(0L)
  e <- emp_dummies(agent); pt <- pt_dummies(agent)
  age_n <- agent$age / 80; dist <- agent$dist_primary_km / 50
  acc_h <- agent$accessibility_home
  V0 <- 0
  V1 <- (  1.50 + 2.50*e$d_fulltime + 1.80*e$d_parttime_hi + 1.20*e$d_parttime_lo
          - 0.80*age_n + 0.30*agent$in_management - 0.20*agent$children_in_hh
          + 0.20*agent$car_avail + 0.10*pt$d_halbtax + 0.15*pt$d_verbund
          + 0.20*pt$d_general - 0.30*dist + 0.25*acc_h )
  V2 <- ( -1.00 + 1.20*e$d_fulltime + 0.80*e$d_parttime_hi + 0.40*e$d_parttime_lo
          - 1.00*age_n + 0.40*agent$in_management - 0.40*agent$children_in_hh
          + 0.30*agent$car_avail + 0.05*pt$d_halbtax + 0.10*pt$d_verbund
          + 0.15*pt$d_general - 0.50*dist + 0.20*acc_h )
  as.integer(mnl_choice(c("0"=V0, "1"=V1, "2"=V2)))
}

# --- 3b. Education tour frequency (0/1/2) ---
education_tour_freq <- function(agent) {
  if (agent$in_education == 0) return(0L)
  e <- emp_dummies(agent); pt <- pt_dummies(agent)
  age_n <- agent$age / 80; dist <- agent$dist_primary_km / 50
  acc_h <- agent$accessibility_home
  V0 <- 0
  V1 <- (  2.00 - 0.40*e$d_fulltime - 0.20*e$d_parttime_hi - 0.10*e$d_parttime_lo
          + 1.50*agent$in_education - 0.40*age_n + 0.15*agent$car_avail
          + 0.15*pt$d_halbtax + 0.25*pt$d_verbund + 0.30*pt$d_general
          - 0.20*dist + 0.20*acc_h )
  V2 <- ( -0.80 - 0.60*e$d_fulltime - 0.30*e$d_parttime_hi - 0.15*e$d_parttime_lo
          + 0.80*agent$in_education - 0.60*age_n + 0.10*agent$car_avail
          + 0.10*pt$d_halbtax + 0.20*pt$d_verbund + 0.25*pt$d_general
          - 0.40*dist + 0.15*acc_h )
  as.integer(mnl_choice(c("0"=V0, "1"=V1, "2"=V2)))
}

# --- 3c. Business tour frequency (0/1/2) ---
# [FIX DIV-4] Added dist (distance to primary) and acc_p (accessibility_primary)
# per Table 1 — both were missing from the previous version.
business_tour_freq <- function(agent, n_primary, has_work_tour) {
  e <- emp_dummies(agent); pt <- pt_dummies(agent)
  age_n <- agent$age / 80; dist <- agent$dist_primary_km / 50
  acc_h <- agent$accessibility_home; acc_p <- agent$accessibility_primary
  V0 <- 0
  V1 <- ( -0.50 + 0.90*e$d_fulltime + 0.60*e$d_parttime_hi + 0.30*e$d_parttime_lo
          - 0.40*age_n - 0.10*agent$children_in_hh + 0.30*agent$car_avail
          + 0.10*pt$d_halbtax + 0.15*pt$d_verbund + 0.20*pt$d_general
          - 0.50*n_primary + 0.40*has_work_tour - 0.25*dist
          + 0.20*acc_h + 0.15*acc_p )
  V2 <- ( -2.00 + 0.60*e$d_fulltime + 0.30*e$d_parttime_hi + 0.10*e$d_parttime_lo
          - 0.60*age_n - 0.20*agent$children_in_hh + 0.40*agent$car_avail
          + 0.05*pt$d_halbtax + 0.10*pt$d_verbund + 0.15*pt$d_general
          - 0.80*n_primary + 0.50*has_work_tour - 0.40*dist
          + 0.15*acc_h + 0.10*acc_p )
  as.integer(mnl_choice(c("0"=V0, "1"=V1, "2"=V2)))
}

# --- 3d. Other tour frequency (0/1/2) ---
# [FIX DIV-5] Added in_edu (main occ. in education) per Table 1.
other_tour_freq <- function(agent, n_primary, n_business, has_work_tour) {
  e <- emp_dummies(agent); pt <- pt_dummies(agent)
  age_n  <- agent$age / 80; dist <- agent$dist_primary_km / 50
  in_edu <- agent$in_education
  acc_h  <- agent$accessibility_home
  n_tours_so_far <- n_primary + n_business
  V0 <- 0
  V1 <- (  0.20 + 0.20*e$d_fulltime + 0.30*e$d_parttime_hi + 0.40*e$d_parttime_lo
          - 0.20*age_n + 0.25*in_edu + 0.30*agent$children_in_hh + 0.25*agent$car_avail
          + 0.10*pt$d_halbtax + 0.20*pt$d_verbund + 0.25*pt$d_general
          - 0.30*dist - 0.40*n_tours_so_far - 0.30*n_primary
          - 0.20*has_work_tour + 0.25*acc_h )
  V2 <- ( -1.00 + 0.10*e$d_fulltime + 0.20*e$d_parttime_hi + 0.30*e$d_parttime_lo
          - 0.40*age_n + 0.15*in_edu + 0.40*agent$children_in_hh + 0.30*agent$car_avail
          + 0.05*pt$d_halbtax + 0.15*pt$d_verbund + 0.20*pt$d_general
          - 0.50*dist - 0.70*n_tours_so_far - 0.50*n_primary
          - 0.30*has_work_tour + 0.20*acc_h )
  as.integer(mnl_choice(c("0"=V0, "1"=V1, "2"=V2)))
}

# =============================================================================
# 4. STOP FREQUENCY — 4 separate MNL models (Table 1, Section 2.4)
# =============================================================================

# --- 4a. Outbound stop frequency (0/1/2) ---
# [FIX DIV-3] Removed acc_p (accessibility_primary): Table 1 does not include
# it for the outbound stop model (only business tour freq and subtour use acc_p).
outbound_stop_freq <- function(agent, n_tours, is_work_tour) {
  e <- emp_dummies(agent); pt <- pt_dummies(agent)
  age_n <- agent$age / 80; dist <- agent$dist_primary_km / 50
  acc_h <- agent$accessibility_home
  V0 <- 0
  V1 <- ( -0.50 + 0.20*e$d_fulltime + 0.40*e$d_parttime_hi + 0.50*e$d_parttime_lo
          - 0.30*age_n + 0.60*agent$children_in_hh + 0.25*agent$car_avail
          + 0.10*pt$d_halbtax + 0.15*pt$d_verbund + 0.20*pt$d_general
          - 0.40*dist - 0.20*n_tours - 0.20*is_work_tour + 0.15*acc_h )
  V2 <- ( -2.00 + 0.10*e$d_fulltime + 0.20*e$d_parttime_hi + 0.30*e$d_parttime_lo
          - 0.50*age_n + 0.80*agent$children_in_hh + 0.30*agent$car_avail
          + 0.05*pt$d_halbtax + 0.10*pt$d_verbund + 0.15*pt$d_general
          - 0.60*dist - 0.40*n_tours - 0.30*is_work_tour + 0.10*acc_h )
  as.integer(mnl_choice(c("0"=V0, "1"=V1, "2"=V2)))
}

# --- 4b. Inbound stop frequency (0/1/2) ---
# [FIX DIV-3] Removed acc_p (accessibility_primary): Table 1 does not include
# it for the inbound stop model (only business tour freq and subtour use acc_p).
inbound_stop_freq <- function(agent, n_tours, is_work_tour) {
  e <- emp_dummies(agent); pt <- pt_dummies(agent)
  age_n <- agent$age / 80; dist <- agent$dist_primary_km / 50
  acc_h <- agent$accessibility_home
  V0 <- 0
  V1 <- ( -0.20 + 0.30*e$d_fulltime + 0.45*e$d_parttime_hi + 0.55*e$d_parttime_lo
          - 0.30*age_n + 0.50*agent$children_in_hh + 0.30*agent$car_avail
          + 0.10*pt$d_halbtax + 0.20*pt$d_verbund + 0.25*pt$d_general
          - 0.35*dist - 0.20*n_tours + 0.20*is_work_tour + 0.20*acc_h )
  V2 <- ( -1.80 + 0.20*e$d_fulltime + 0.30*e$d_parttime_hi + 0.40*e$d_parttime_lo
          - 0.50*age_n + 0.60*agent$children_in_hh + 0.35*agent$car_avail
          + 0.05*pt$d_halbtax + 0.15*pt$d_verbund + 0.20*pt$d_general
          - 0.55*dist - 0.40*n_tours + 0.25*is_work_tour + 0.15*acc_h )
  as.integer(mnl_choice(c("0"=V0, "1"=V1, "2"=V2)))
}

# --- 4c. Secondary tour stop frequency (0/1/2) ---
#
# [FIX DIV-1, DIV-7] ONE single MNL per Table 1. Predicts TOTAL number of
# stops for secondary (Business/Other) tours. Variables match Table 1 exactly:
#   constant, employment, age, children_in_HH, car_avail, PT subscription,
#   no_of_tours, is_business_tour (binary), accessibility_home.
# The paper gives NO outbound/inbound split for secondary tours; the previous
# 8-cell architecture was invented. All beta coefficients are invented
# (directionally plausible); not estimated from MZ 2015.
#
# Stop split convention (applied in the main loop):
#   n_stops = 0 → no stops
#   n_stops = 1 → 1 inbound stop (after primary activity)
#   n_stops = 2 → 1 outbound stop + 1 inbound stop

secondary_stop_freq <- function(agent, is_business_tour, n_tours) {
  e     <- emp_dummies(agent); pt <- pt_dummies(agent)
  age_n <- agent$age / 80
  acc_h <- agent$accessibility_home
  d_bus <- as.integer(is_business_tour)
  V0 <- 0
  V1 <- ( -0.80 + 0.30*e$d_fulltime + 0.50*e$d_parttime_hi + 0.60*e$d_parttime_lo
          - 0.30*age_n + 0.40*agent$children_in_hh + 0.25*agent$car_avail
          + 0.10*pt$d_halbtax + 0.20*pt$d_verbund + 0.25*pt$d_general
          - 0.25*n_tours - 0.20*d_bus + 0.20*acc_h )
  V2 <- ( -2.20 + 0.15*e$d_fulltime + 0.30*e$d_parttime_hi + 0.40*e$d_parttime_lo
          - 0.50*age_n + 0.50*agent$children_in_hh + 0.30*agent$car_avail
          + 0.05*pt$d_halbtax + 0.15*pt$d_verbund + 0.20*pt$d_general
          - 0.50*n_tours - 0.30*d_bus + 0.15*acc_h )
  as.integer(mnl_choice(c("0"=V0, "1"=V1, "2"=V2)))
}

# --- 4d. Subtour (work -> X -> work) ---
# Hard constraint: subtour requires a primary work tour (Section 2.4)
# [FIX DIV-6] Removed dist and acc_h (absent from Table 1 subtour column);
# added in_edu (main occ. in education, present in Table 1); kept acc_p.
has_subtour <- function(agent, n_tours, is_work_tour) {
  if (!is_work_tour) return(FALSE)
  e <- emp_dummies(agent); pt <- pt_dummies(agent)
  age_n  <- agent$age / 80
  in_edu <- agent$in_education
  acc_p  <- agent$accessibility_primary
  V_no  <- 0
  V_yes <- ( -2.00 + 0.50*e$d_fulltime + 0.30*e$d_parttime_hi + 0.10*e$d_parttime_lo
             - 0.40*age_n + 0.20*in_edu - 0.30*agent$children_in_hh + 0.30*agent$car_avail
             + 0.10*pt$d_halbtax + 0.15*pt$d_verbund + 0.20*pt$d_general
             - 0.30*n_tours + 0.40*is_work_tour + 0.30*acc_p )
  mnl_choice(c("no"=V_no, "yes"=V_yes)) == "yes"
}

# =============================================================================
# 5. ACTIVITY TYPE ASSIGNMENT (Section 2.4)
#    Segmented by person group, direction and tour type.
#    Probabilities approximated — exact tables not published in paper.
# =============================================================================

assign_activity_type <- function(agent, direction = "inbound", tour_type = "other") {

  # Subtour check MUST come first: subtours are only made by workers, so those
  # agents would be caught by is_fulltime/is_parttime below and the subtour
  # branch would be unreachable. This was a bug in the original code (the call
  # site was also passing direction="inbound" instead of direction="subtour").
  if (direction == "subtour") {
    probs <- c(Leisure  = 0.40, Shopping = 0.30, Business = 0.15,
               Education= 0.00, Accompany= 0.05, Other    = 0.10)
    return(sample(names(probs), 1, prob = probs))
  }

  # [FIX UA-1] "primary" direction: purpose-neutral probabilities for the main
  # activity of a secondary (Other) tour. Previously called with direction="inbound"
  # which biased toward shopping-like weights. A neutral table is more appropriate.
  if (direction == "primary") {
    if (agent$in_education == 1) {
      probs <- c(Leisure=0.40, Shopping=0.20, Business=0.00,
                 Education=0.05, Accompany=0.25, Other=0.10)
    } else if (agent$employment == "full_time") {
      probs <- c(Leisure=0.30, Shopping=0.25, Business=0.18,
                 Education=0.00, Accompany=0.12, Other=0.15)
    } else if (agent$employment %in% c("part_time_hi","part_time_lo")) {
      probs <- c(Leisure=0.38, Shopping=0.30, Business=0.08,
                 Education=0.00, Accompany=0.12, Other=0.12)
    } else {
      probs <- c(Leisure=0.45, Shopping=0.35, Business=0.00,
                 Education=0.00, Accompany=0.10, Other=0.10)
    }
    return(sample(names(probs), 1, prob = probs))
  }

  is_student    <- agent$in_education == 1
  is_fulltime   <- agent$employment == "full_time"
  is_parttime   <- agent$employment %in% c("part_time_hi","part_time_lo")
  is_unemployed <- agent$employment == "not_employed" & agent$in_education == 0

  if (is_student) {
    probs <- if (direction == "outbound")
      c(Leisure=0.35, Shopping=0.10, Business=0.00, Education=0.05, Accompany=0.35, Other=0.15)
    else
      c(Leisure=0.50, Shopping=0.15, Business=0.00, Education=0.05, Accompany=0.15, Other=0.15)

  } else if (is_fulltime) {
    probs <- if (direction == "outbound" & tour_type == "work")
      c(Leisure=0.08, Shopping=0.12, Business=0.22, Education=0.00, Accompany=0.42, Other=0.16)
    else if (direction == "inbound" & tour_type == "work")
      c(Leisure=0.20, Shopping=0.35, Business=0.15, Education=0.00, Accompany=0.15, Other=0.15)
    else
      c(Leisure=0.30, Shopping=0.28, Business=0.15, Education=0.00, Accompany=0.12, Other=0.15)

  } else if (is_parttime) {
    probs <- if (direction == "outbound" & tour_type == "work")
      c(Leisure=0.12, Shopping=0.18, Business=0.12, Education=0.00, Accompany=0.38, Other=0.20)
    else if (direction == "inbound" & tour_type == "work")
      c(Leisure=0.28, Shopping=0.35, Business=0.08, Education=0.00, Accompany=0.14, Other=0.15)
    else
      c(Leisure=0.35, Shopping=0.30, Business=0.08, Education=0.00, Accompany=0.12, Other=0.15)

  } else if (is_unemployed) {
    # Retired/unemployed: entirely discretionary, no business or education stops
    probs <- c(Leisure=0.45, Shopping=0.35, Business=0.00, Education=0.00,
               Accompany=0.10, Other=0.10)
  } else {
    probs <- c(Leisure=0.35, Shopping=0.25, Business=0.10, Education=0.02,
               Accompany=0.13, Other=0.15)
  }

  sample(names(probs), 1, prob = probs)
}

# =============================================================================
# TESTING SECTION — Part 1
# Run this block to test each function on a small subset of agents.
# Produces agent_test: one row per agent, one column per model output.
# You can inspect it with head(agent_test) or View(agent_test).
# =============================================================================

N_TEST <- 30   # number of agents to test on

agent_test <- pop[1:N_TEST, ]

# --- Tour frequency models ---
agent_test$work_tour_freq <- sapply(1:N_TEST, function(i)
  work_tour_freq(agent_test[i, ]))

agent_test$ed_tour_freq <- sapply(1:N_TEST, function(i)
  education_tour_freq(agent_test[i, ]))

# Intermediate variables needed as inputs to downstream models
agent_test_n_primary     <- agent_test$work_tour_freq + agent_test$ed_tour_freq
agent_test_has_work_tour <- as.numeric(agent_test$work_tour_freq > 0)

agent_test$business_tour_freq <- sapply(1:N_TEST, function(i)
  business_tour_freq(agent_test[i, ],
                     agent_test_n_primary[i],
                     agent_test_has_work_tour[i]))

agent_test_n_tours_so_far <- agent_test_n_primary + agent_test$business_tour_freq

agent_test$other_tour_freq <- sapply(1:N_TEST, function(i)
  other_tour_freq(agent_test[i, ],
                  agent_test_n_primary[i],
                  agent_test$business_tour_freq[i],
                  agent_test_has_work_tour[i]))

# Total tours per agent
agent_test_n_tours      <- agent_test$work_tour_freq + agent_test$ed_tour_freq +
                           agent_test$business_tour_freq + agent_test$other_tour_freq
agent_test_is_work_tour <- as.numeric(agent_test$work_tour_freq > 0)

# --- Stop frequency models ---
agent_test$outbound_stop_freq <- sapply(1:N_TEST, function(i)
  outbound_stop_freq(agent_test[i, ],
                     agent_test_n_tours[i],
                     agent_test_is_work_tour[i]))

agent_test$inbound_stop_freq <- sapply(1:N_TEST, function(i)
  inbound_stop_freq(agent_test[i, ],
                    agent_test_n_tours[i],
                    agent_test_is_work_tour[i]))

agent_test_is_business_tour <- as.numeric(agent_test$business_tour_freq > 0)

# [FIX DIV-1/7] Test new single-MNL secondary_stop_freq() with is_business_tour flag.
agent_test$sec_stop_bus <- sapply(1:N_TEST, function(i)
  secondary_stop_freq(agent_test[i, ], is_business_tour = TRUE,
                      agent_test_n_tours[i]))

agent_test$sec_stop_other <- sapply(1:N_TEST, function(i)
  secondary_stop_freq(agent_test[i, ], is_business_tour = FALSE,
                      agent_test_n_tours[i]))

agent_test$has_subtour <- sapply(1:N_TEST, function(i)
  has_subtour(agent_test[i, ],
              agent_test_n_tours[i],
              agent_test_is_work_tour[i]))

# --- Activity type assignment (one example stop per agent) ---
# Tested for outbound work stop — change direction/tour_type to explore others
agent_test$activity_type_example <- sapply(1:N_TEST, function(i)
  assign_activity_type(agent_test[i, ],
                       direction  = "outbound",
                       tour_type  = "work"))

cat("=== agent_test dataframe (Part 1) ===\n")
print(head(agent_test, 10))
cat(sprintf("\nTour frequency check — agents with no primary tour: %d / %d\n",
    sum(agent_test_n_primary == 0), N_TEST))
cat(sprintf("Agents with subtour: %d / %d\n",
    sum(agent_test$has_subtour), N_TEST))
# [FIX DIV-1/7] Sanity check on new single-MNL secondary_stop_freq()
cat(sprintf("Mean sec stops — business tours : %.2f\n",
    mean(agent_test$sec_stop_bus)))
cat(sprintf("Mean sec stops — other tours    : %.2f  (should be slightly higher)\n",
    mean(agent_test$sec_stop_other)))
cat("\n")

# =============================================================================
# RUN PART 1: Apply tour/stop/activity choices for each agent
# =============================================================================

cat("Running tour & stop frequency models for all agents...\n")

agent_tours <- vector("list", N_AGENTS)

for (i in 1:N_AGENTS) {
  agent <- pop[i, ]

  # Tour frequency
  n_work  <- work_tour_freq(agent)
  n_edu   <- education_tour_freq(agent)
  n_prim  <- n_work + n_edu
  has_work_tour <- n_work > 0
  n_bus   <- business_tour_freq(agent, n_prim, has_work_tour)
  n_other <- other_tour_freq(agent, n_prim, n_bus, has_work_tour)
  n_tours <- n_prim + n_bus + n_other

  # Build activity list tour by tour
  tours <- list()

  for (t in seq_len(n_work)) {
    n_out   <- outbound_stop_freq(agent, n_tours, is_work_tour = TRUE)
    n_in    <- inbound_stop_freq(agent,  n_tours, is_work_tour = TRUE)
    subtour <- has_subtour(agent, n_tours, is_work_tour = TRUE)
    acts <- character(0)
    if (n_out > 0) acts <- c(acts, replicate(n_out, assign_activity_type(agent, "outbound", "work")))
    acts <- c(acts, "Work")
    # [CHANGE 1 – Part 1] Record which position in acts is the subtour activity
    # so Part 2 can apply a mode constrained by the main tour mode for that
    # activity only. NA when no subtour occurs.
    subtour_idx <- NA_integer_
    if (subtour) {
      acts <- c(acts, assign_activity_type(agent, "subtour", "work"))
      subtour_idx <- length(acts)   # index of the subtour activity in acts
    }
    if (n_in > 0)  acts <- c(acts, replicate(n_in,  assign_activity_type(agent, "inbound",  "work")))
    acts <- as.character(unlist(acts))
    tours[[length(tours)+1]] <- list(type="work", activities=acts, subtour_idx=subtour_idx)
  }

  for (t in seq_len(n_edu)) {
    n_out <- outbound_stop_freq(agent, n_tours, is_work_tour = FALSE)
    n_in  <- inbound_stop_freq(agent,  n_tours, is_work_tour = FALSE)
    acts  <- character(0)
    if (n_out > 0) acts <- c(acts, replicate(n_out, assign_activity_type(agent, "outbound", "education")))
    acts <- c(acts, "Education")
    if (n_in > 0)  acts <- c(acts, replicate(n_in,  assign_activity_type(agent, "inbound",  "education")))
    acts <- as.character(unlist(acts))
    tours[[length(tours)+1]] <- list(type="education", activities=acts, subtour_idx=NA_integer_)
  }

  for (t in seq_len(n_bus)) {
    # [FIX DIV-1/7] Single MNL predicts TOTAL stops; split: n=1 → inbound,
    # n=2 → outbound + inbound. No mode proxy needed (mode is not in Table 1).
    n_stops <- secondary_stop_freq(agent, is_business_tour = TRUE, n_tours)
    n_out   <- if (n_stops >= 2L) 1L else 0L
    n_in    <- if (n_stops >= 1L) 1L else 0L
    acts  <- character(0)
    if (n_out > 0) acts <- c(acts, assign_activity_type(agent, "outbound", "business"))
    acts <- c(acts, "Business")
    if (n_in  > 0) acts <- c(acts, assign_activity_type(agent, "inbound",  "business"))
    acts <- as.character(unlist(acts))
    tours[[length(tours)+1]] <- list(type="business", activities=acts, subtour_idx=NA_integer_)
  }

  for (t in seq_len(n_other)) {
    # [FIX UA-1] primary_act now uses direction="primary" (neutral probabilities)
    # instead of the old "inbound" which biased stop-type composition.
    # [FIX DIV-1/7] Single MNL total stops; split: n=1 → inbound, n=2 → out+in.
    primary_act <- assign_activity_type(agent, "primary", "other")
    n_stops <- secondary_stop_freq(agent, is_business_tour = FALSE, n_tours)
    n_out   <- if (n_stops >= 2L) 1L else 0L
    n_in    <- if (n_stops >= 1L) 1L else 0L
    acts  <- character(0)
    if (n_out > 0) acts <- c(acts, assign_activity_type(agent, "outbound", "other"))
    acts <- c(acts, primary_act)
    if (n_in  > 0) acts <- c(acts, assign_activity_type(agent, "inbound",  "other"))
    acts <- as.character(unlist(acts))
    tours[[length(tours)+1]] <- list(type="other", activities=acts, subtour_idx=NA_integer_)
  }

  if (length(tours) == 0)
    tours <- list(list(type="other", activities="Leisure", subtour_idx=NA_integer_))

  agent_tours[[i]] <- list(
    n_work  = n_work,
    n_edu   = n_edu,
    n_bus   = n_bus,
    n_other = n_other,
    n_tours = n_tours,
    tours   = tours
  )
}

cat("Done.\n\n")

# Quick summary
cat("=== Part 1: Tour frequency summary ===\n")
cat(sprintf("Mean work tours    : %.2f\n", mean(sapply(agent_tours, function(x) x$n_work))))
cat(sprintf("Mean edu tours     : %.2f\n", mean(sapply(agent_tours, function(x) x$n_edu))))
cat(sprintf("Mean business tours: %.2f\n", mean(sapply(agent_tours, function(x) x$n_bus))))
cat(sprintf("Mean other tours   : %.2f\n", mean(sapply(agent_tours, function(x) x$n_other))))
cat(sprintf("Mean total tours   : %.2f  [Paper: 1.49]\n",
    mean(sapply(agent_tours, function(x) x$n_tours))))
cat("\n")

# =============================================================================
# SAVE OUTPUT for Part 2
# =============================================================================

save(pop, agent_tours, N_AGENTS,
     # Save all functions needed by later parts
     mnl_choice, emp_dummies, pt_dummies,
     work_tour_freq, education_tour_freq, business_tour_freq, other_tour_freq,
     outbound_stop_freq, inbound_stop_freq, secondary_stop_freq,
     has_subtour, assign_activity_type,
     file = "part1_output.RData")

cat("Part 1 complete. Output saved to part1_output.RData\n")
cat("Run part2_destination_mode.R next.\n")
