# =============================================================================
# Replication of SIMBA MOBi Agent Activity Simulation
# Based on: Scherr et al. (2020), EJTIR 20(4), pp. 152-172
# "Towards agent-based travel demand simulation across all mobility choices"
#
# This script simulates the MOBi.plans module for N synthetic agents:
#   1.  Synthetic population generation
#   2.  Long-term choices (car availability, PT subscription)
#   3.  Tour frequency — 4 separate MNL models (Table 1):
#         work tours, education tours, business tours, other tours
#   4.  Stop frequency — 4 separate MNL models (Table 1):
#         outbound stops, inbound stops, secondary stops, subtour
#   5.  Activity type assignment (segmented by person group)
#   6.  Destination & mode choice (Nested Logit with logsum, Eq. 1-5)
#   7.  Activity duration sampling (Figure 2)
#   8.  Rule-based plan-building with time budgets (Table 3, Section 2.5)
#   9.  Activity start-time scheduling (Figure 3, Section 2.5)
#   10. Validation summary (Table 4 analog)
#   11. Plots
#
# IMPORTANT NOTE ON COEFFICIENTS:
#   The paper specifies which variables enter each MNL model (Table 1)
#   but does not publish the estimated beta values — these were estimated
#   using Biogeme from the Swiss MZ 2015 travel diary survey (not public).
#   All beta coefficients in this script are invented, chosen to be
#   directionally plausible. Variable inclusion strictly follows Table 1.
#   Reference categories: employment = "not_employed", PT sub = "none"
# =============================================================================

set.seed(42)
library(dplyr)
library(tidyr)
library(ggplot2)

N_AGENTS <- 100   # change to any number

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
    # Distance to primary location in km (work/school)
    dist_primary_km       = pmax(1, rnorm(n, mean = 12, sd = 8)),
    # Accessibility scores [0,1] — home and primary location
    # In the real model these come from the transport network (logsums)
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

# =============================================================================
# HELPER: Generic MNL choice
# =============================================================================

mnl_choice <- function(utils) {
  # Subtract max for numerical stability before exponentiation
  exp_u <- exp(utils - max(utils))
  probs  <- exp_u / sum(exp_u)
  sample(names(utils), 1, prob = probs)
}

# =============================================================================
# HELPER: Dummy variable extractors
# All MNL models use dummies with:
#   reference employment  = "not_employed"
#   reference PT sub      = "none"
# =============================================================================

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
#    work_tour_freq()      : 0/1/2 work tours
#    education_tour_freq() : 0/1/2 education tours
#    business_tour_freq()  : 0/1/2 business tours
#    other_tour_freq()     : 0/1/2 other tours
# =============================================================================

# --- 3a. Work tour frequency ---
work_tour_freq <- function(agent) {
  
  # Hard constraint: must be employed
  if (!agent$employment %in% c("full_time","part_time_hi","part_time_lo"))
    return(0L)
  
  e     <- emp_dummies(agent)
  pt    <- pt_dummies(agent)
  age_n <- agent$age / 80
  dist  <- agent$dist_primary_km / 50
  acc_h <- agent$accessibility_home
  
  V0 <- 0
  
  V1 <- (  1.50
           + 2.50 * e$d_fulltime
           + 1.80 * e$d_parttime_hi
           + 1.20 * e$d_parttime_lo
           - 0.80 * age_n
           + 0.30 * agent$in_management
           - 0.20 * agent$children_in_hh
           + 0.20 * agent$car_avail
           + 0.10 * pt$d_halbtax
           + 0.15 * pt$d_verbund
           + 0.20 * pt$d_general
           - 0.30 * dist
           + 0.25 * acc_h )
  
  V2 <- ( -1.00
          + 1.20 * e$d_fulltime
          + 0.80 * e$d_parttime_hi
          + 0.40 * e$d_parttime_lo
          - 1.00 * age_n
          + 0.40 * agent$in_management
          - 0.40 * agent$children_in_hh
          + 0.30 * agent$car_avail
          + 0.05 * pt$d_halbtax
          + 0.10 * pt$d_verbund
          + 0.15 * pt$d_general
          - 0.50 * dist
          + 0.20 * acc_h )
  
  as.integer(mnl_choice(c("0"=V0, "1"=V1, "2"=V2)))
}

# --- 3b. Education tour frequency ---
education_tour_freq <- function(agent) {
  
  # Hard constraint: must be in education
  if (agent$in_education == 0) return(0L)
  
  e     <- emp_dummies(agent)
  pt    <- pt_dummies(agent)
  age_n <- agent$age / 80
  dist  <- agent$dist_primary_km / 50
  acc_h <- agent$accessibility_home
  
  V0 <- 0
  
  V1 <- (  2.00
           - 0.40 * e$d_fulltime       # working alongside studying reduces tours
           - 0.20 * e$d_parttime_hi
           - 0.10 * e$d_parttime_lo
           + 1.50 * agent$in_education  # strong effect if main occupation
           - 0.40 * age_n
           + 0.15 * agent$car_avail
           + 0.15 * pt$d_halbtax
           + 0.25 * pt$d_verbund
           + 0.30 * pt$d_general
           - 0.20 * dist
           + 0.20 * acc_h )
  
  V2 <- ( -0.80
          - 0.60 * e$d_fulltime
          - 0.30 * e$d_parttime_hi
          - 0.15 * e$d_parttime_lo
          + 0.80 * agent$in_education
          - 0.60 * age_n
          + 0.10 * agent$car_avail
          + 0.10 * pt$d_halbtax
          + 0.20 * pt$d_verbund
          + 0.25 * pt$d_general
          - 0.40 * dist
          + 0.15 * acc_h )
  
  as.integer(mnl_choice(c("0"=V0, "1"=V1, "2"=V2)))
}

# --- 3c. Business tour frequency ---
# n_primary    : number of primary tours already planned
# has_work_tour: whether agent has a work primary tour (boolean)
business_tour_freq <- function(agent, n_primary, has_work_tour) {
  
  e     <- emp_dummies(agent)
  pt    <- pt_dummies(agent)
  age_n <- agent$age / 80
  acc_h <- agent$accessibility_home
  
  V0 <- 0
  
  V1 <- ( -0.50
          + 0.90 * e$d_fulltime
          + 0.60 * e$d_parttime_hi
          + 0.30 * e$d_parttime_lo
          # ref (not_employed): lowest probability of business tours
          - 0.40 * age_n
          - 0.10 * agent$children_in_hh
          + 0.30 * agent$car_avail
          + 0.10 * pt$d_halbtax
          + 0.15 * pt$d_verbund
          + 0.20 * pt$d_general
          - 0.50 * n_primary           # more primary tours = less time left
          + 0.40 * has_work_tour       # work tours generate business activity
          + 0.20 * acc_h )
  
  V2 <- ( -2.00
          + 0.60 * e$d_fulltime
          + 0.30 * e$d_parttime_hi
          + 0.10 * e$d_parttime_lo
          - 0.60 * age_n
          - 0.20 * agent$children_in_hh
          + 0.40 * agent$car_avail
          + 0.05 * pt$d_halbtax
          + 0.10 * pt$d_verbund
          + 0.15 * pt$d_general
          - 0.80 * n_primary
          + 0.50 * has_work_tour
          + 0.15 * acc_h )
  
  as.integer(mnl_choice(c("0"=V0, "1"=V1, "2"=V2)))
}

# --- 3d. Other tour frequency ---
# n_primary   : number of primary tours
# n_business  : number of business tours already planned
# has_work_tour: whether agent has a work primary tour
other_tour_freq <- function(agent, n_primary, n_business, has_work_tour) {
  
  e              <- emp_dummies(agent)
  pt             <- pt_dummies(agent)
  age_n          <- agent$age / 80
  dist           <- agent$dist_primary_km / 50
  acc_h          <- agent$accessibility_home
  n_tours_so_far <- n_primary + n_business
  
  V0 <- 0
  
  V1 <- (  0.20
           + 0.20 * e$d_fulltime
           + 0.30 * e$d_parttime_hi
           + 0.40 * e$d_parttime_lo
           # ref (not_employed): highest base for other tours — all day discretionary
           - 0.20 * age_n
           + 0.30 * agent$children_in_hh
           + 0.25 * agent$car_avail
           + 0.10 * pt$d_halbtax
           + 0.20 * pt$d_verbund
           + 0.25 * pt$d_general
           - 0.30 * dist
           - 0.40 * n_tours_so_far      # already committed time
           - 0.30 * n_primary
           - 0.20 * has_work_tour
           + 0.25 * acc_h )
  
  V2 <- ( -1.00
          + 0.10 * e$d_fulltime
          + 0.20 * e$d_parttime_hi
          + 0.30 * e$d_parttime_lo
          - 0.40 * age_n
          + 0.40 * agent$children_in_hh
          + 0.30 * agent$car_avail
          + 0.05 * pt$d_halbtax
          + 0.15 * pt$d_verbund
          + 0.20 * pt$d_general
          - 0.50 * dist
          - 0.70 * n_tours_so_far
          - 0.50 * n_primary
          - 0.30 * has_work_tour
          + 0.20 * acc_h )
  
  as.integer(mnl_choice(c("0"=V0, "1"=V1, "2"=V2)))
}

# =============================================================================
# 4. STOP FREQUENCY — 4 separate MNL models (Table 1, Section 2.4)
#
#    outbound_stop_freq()  : stops between home and primary activity
#    inbound_stop_freq()   : stops between primary activity and home
#    secondary_stop_freq() : stops within a secondary tour
#    has_subtour()         : whether a work->X->work subtour exists
# =============================================================================

# --- 4a. Outbound stop frequency ---
# n_tours      : total tours planned so far
# is_work_tour : whether this is a work primary tour
outbound_stop_freq <- function(agent, n_tours, is_work_tour) {
  
  e     <- emp_dummies(agent)
  pt    <- pt_dummies(agent)
  age_n <- agent$age / 80
  dist  <- agent$dist_primary_km / 50
  acc_h <- agent$accessibility_home
  acc_p <- agent$accessibility_primary
  
  V0 <- 0
  
  V1 <- ( -0.50
          + 0.20 * e$d_fulltime
          + 0.40 * e$d_parttime_hi
          + 0.50 * e$d_parttime_lo
          - 0.30 * age_n
          + 0.60 * agent$children_in_hh  # morning drop-off stops
          + 0.25 * agent$car_avail
          + 0.10 * pt$d_halbtax
          + 0.15 * pt$d_verbund
          + 0.20 * pt$d_general
          - 0.40 * dist                  # long distance = less detour
          - 0.20 * n_tours
          - 0.20 * is_work_tour          # work tours more structured
          + 0.15 * acc_h
          + 0.10 * acc_p )
  
  V2 <- ( -2.00
          + 0.10 * e$d_fulltime
          + 0.20 * e$d_parttime_hi
          + 0.30 * e$d_parttime_lo
          - 0.50 * age_n
          + 0.80 * agent$children_in_hh
          + 0.30 * agent$car_avail
          + 0.05 * pt$d_halbtax
          + 0.10 * pt$d_verbund
          + 0.15 * pt$d_general
          - 0.60 * dist
          - 0.40 * n_tours
          - 0.30 * is_work_tour
          + 0.10 * acc_h
          + 0.10 * acc_p )
  
  as.integer(mnl_choice(c("0"=V0, "1"=V1, "2"=V2)))
}

# --- 4b. Inbound stop frequency ---
inbound_stop_freq <- function(agent, n_tours, is_work_tour) {
  
  e     <- emp_dummies(agent)
  pt    <- pt_dummies(agent)
  age_n <- agent$age / 80
  dist  <- agent$dist_primary_km / 50
  acc_h <- agent$accessibility_home
  acc_p <- agent$accessibility_primary
  
  V0 <- 0
  
  V1 <- ( -0.20                         # inbound stops more common than outbound
          + 0.30 * e$d_fulltime
          + 0.45 * e$d_parttime_hi
          + 0.55 * e$d_parttime_lo
          - 0.30 * age_n
          + 0.50 * agent$children_in_hh  # afternoon pick-up stops
          + 0.30 * agent$car_avail
          + 0.10 * pt$d_halbtax
          + 0.20 * pt$d_verbund
          + 0.25 * pt$d_general
          - 0.35 * dist
          - 0.20 * n_tours
          + 0.20 * is_work_tour          # shopping on way home from work
          + 0.20 * acc_h
          + 0.15 * acc_p )
  
  V2 <- ( -1.80
          + 0.20 * e$d_fulltime
          + 0.30 * e$d_parttime_hi
          + 0.40 * e$d_parttime_lo
          - 0.50 * age_n
          + 0.60 * agent$children_in_hh
          + 0.35 * agent$car_avail
          + 0.05 * pt$d_halbtax
          + 0.15 * pt$d_verbund
          + 0.20 * pt$d_general
          - 0.55 * dist
          - 0.40 * n_tours
          + 0.25 * is_work_tour
          + 0.15 * acc_h
          + 0.10 * acc_p )
  
  as.integer(mnl_choice(c("0"=V0, "1"=V1, "2"=V2)))
}

# --- 4c. Secondary tour stop frequency ---
# is_business_tour: whether this secondary tour is a business tour
secondary_stop_freq <- function(agent, n_tours, is_business_tour) {
  
  e     <- emp_dummies(agent)
  pt    <- pt_dummies(agent)
  age_n <- agent$age / 80
  acc_h <- agent$accessibility_home
  
  V0 <- 0
  
  V1 <- (  0.00
           + 0.10 * e$d_fulltime
           + 0.20 * e$d_parttime_hi
           + 0.30 * e$d_parttime_lo
           # ref (not_employed): highest base — most flexible for chaining stops
           - 0.20 * age_n
           + 0.30 * agent$children_in_hh
           + 0.25 * agent$car_avail
           + 0.10 * pt$d_halbtax
           + 0.15 * pt$d_verbund
           + 0.20 * pt$d_general
           - 0.30 * n_tours
           - 0.30 * is_business_tour      # business tours more purposeful
           + 0.20 * acc_h )
  
  V2 <- ( -1.50
          + 0.05 * e$d_fulltime
          + 0.15 * e$d_parttime_hi
          + 0.25 * e$d_parttime_lo
          - 0.40 * age_n
          + 0.40 * agent$children_in_hh
          + 0.30 * agent$car_avail
          + 0.05 * pt$d_halbtax
          + 0.10 * pt$d_verbund
          + 0.15 * pt$d_general
          - 0.60 * n_tours
          - 0.50 * is_business_tour
          + 0.15 * acc_h )
  
  as.integer(mnl_choice(c("0"=V0, "1"=V1, "2"=V2)))
}

# --- 4d. Subtour (work -> X -> work) ---
# Only meaningful if agent has a work primary tour
has_subtour <- function(agent, n_tours, is_work_tour) {
  
  # Hard constraint: subtour requires a primary work tour
  if (!is_work_tour) return(FALSE)
  
  e     <- emp_dummies(agent)
  pt    <- pt_dummies(agent)
  age_n <- agent$age / 80
  dist  <- agent$dist_primary_km / 50
  acc_h <- agent$accessibility_home
  acc_p <- agent$accessibility_primary
  
  V_no  <- 0
  
  V_yes <- ( -2.00                       # subtours relatively rare
             + 0.50 * e$d_fulltime
             + 0.30 * e$d_parttime_hi
             + 0.10 * e$d_parttime_lo
             - 0.40 * age_n
             - 0.30 * agent$children_in_hh
             + 0.30 * agent$car_avail     # car makes mid-day trips feasible
             + 0.10 * pt$d_halbtax
             + 0.15 * pt$d_verbund
             + 0.20 * pt$d_general
             - 0.50 * dist                # short commute = easier subtour
             - 0.30 * n_tours
             + 0.40 * is_work_tour        # anchor for the subtour
             + 0.20 * acc_h
             + 0.30 * acc_p )             # good primary accessibility = options nearby
  
  mnl_choice(c("no"=V_no, "yes"=V_yes)) == "yes"
}

# =============================================================================
# 5. ACTIVITY TYPE ASSIGNMENT (Section 2.4)
#    Assigns stop activity types from empirical probability distributions
#    segmented by person group, direction and tour type.
#    Probabilities are approximated — exact tables not published in paper.
#    Reference: Swiss MZ 2015 travel diary survey.
#
#    Activity types: Leisure, Shopping, Business, Education, Accompany, Other
# =============================================================================

assign_activity_type <- function(agent, direction = "inbound", tour_type = "other") {
  
  is_student    <- agent$in_education == 1
  is_fulltime   <- agent$employment == "full_time"
  is_parttime   <- agent$employment %in% c("part_time_hi","part_time_lo")
  is_unemployed <- agent$employment == "not_employed" & agent$in_education == 0
  
  if (is_student) {
    if (direction == "outbound") {
      probs <- c(Leisure=0.35, Shopping=0.10, Business=0.00,
                 Education=0.05, Accompany=0.35, Other=0.15)
    } else {
      # After school: leisure dominates
      probs <- c(Leisure=0.50, Shopping=0.15, Business=0.00,
                 Education=0.05, Accompany=0.15, Other=0.15)
    }
    
  } else if (is_fulltime) {
    if (direction == "outbound" & tour_type == "work") {
      # Morning: drop-off kids + business errands
      probs <- c(Leisure=0.08, Shopping=0.12, Business=0.22,
                 Education=0.00, Accompany=0.42, Other=0.16)
    } else if (direction == "inbound" & tour_type == "work") {
      # Evening: shopping on way home
      probs <- c(Leisure=0.20, Shopping=0.35, Business=0.15,
                 Education=0.00, Accompany=0.15, Other=0.15)
    } else {
      # Secondary tours: leisure and shopping
      probs <- c(Leisure=0.30, Shopping=0.28, Business=0.15,
                 Education=0.00, Accompany=0.12, Other=0.15)
    }
    
  } else if (is_parttime) {
    if (direction == "outbound" & tour_type == "work") {
      probs <- c(Leisure=0.12, Shopping=0.18, Business=0.12,
                 Education=0.00, Accompany=0.38, Other=0.20)
    } else if (direction == "inbound" & tour_type == "work") {
      probs <- c(Leisure=0.28, Shopping=0.35, Business=0.08,
                 Education=0.00, Accompany=0.14, Other=0.15)
    } else {
      probs <- c(Leisure=0.35, Shopping=0.30, Business=0.08,
                 Education=0.00, Accompany=0.12, Other=0.15)
    }
    
  } else if (is_unemployed) {
    # Retired/unemployed: entirely discretionary
    # No business or education stops
    probs <- c(Leisure=0.45, Shopping=0.35, Business=0.00,
               Education=0.00, Accompany=0.10, Other=0.10)
    
  } else {
    # Fallback
    probs <- c(Leisure=0.35, Shopping=0.25, Business=0.10,
               Education=0.02, Accompany=0.13, Other=0.15)
  }
  
  sample(names(probs), 1, prob = probs)
}

# =============================================================================
# 6. DESTINATION & MODE CHOICE — Nested Logit (Equations 1-5, Section 2.4)
# =============================================================================

N_ZONES <- 20

# Simulate travel time matrices (minutes) for each mode
gen_tt_matrix <- function(n_zones) {
  d <- matrix(runif(n_zones^2, 5, 90), n_zones, n_zones)
  diag(d) <- 0
  list(
    walk     = pmin(d * 3.0, 120),
    bicycle  = pmin(d * 0.8,  90),
    pt       = pmin(d * 1.2,  80),
    car      = pmin(d * 0.7,  70),
    car_pass = pmin(d * 0.7,  70)
  )
}

tt <- gen_tt_matrix(N_ZONES)

# Zone attraction (proxy for number of jobs / amenities)
zone_attr <- runif(N_ZONES, 500, 12000)

# Mode utility (Eq. 1, Table 2 variables)
mode_utility <- function(origin, dest, agent, tt_list) {
  asc  <- c(walk=-1.5, bicycle=-0.8, pt=0.0, car_driver=0.5, car_passenger=-0.5)
  beta_tt <- c(walk=-0.06, bicycle=-0.05, pt=-0.04,
               car_driver=-0.03, car_passenger=-0.03)
  
  u <- asc + beta_tt * c(
    tt_list$walk[origin, dest],
    tt_list$bicycle[origin, dest],
    tt_list$pt[origin, dest],
    tt_list$car[origin, dest],
    tt_list$car_pass[origin, dest]
  )
  
  # No car driver if no car available
  if (agent$car_avail == 0) u["car_driver"] <- -Inf
  
  # PT bonus by subscription type
  pt_bonus <- switch(agent$pt_sub,
                     general  = 1.5,
                     verbund  = 0.8,
                     halbtax  = 0.3,
                     none     = 0.0
  )
  u["pt"] <- u["pt"] + pt_bonus
  u
}

# Logsum / Expected Maximum Utility (Eq. 2)
compute_logsum <- function(utils, theta = 1) {
  valid <- utils[is.finite(utils)]
  log(sum(exp((valid - max(valid)) / theta))) + max(valid) / theta
}

# Destination choice (Eq. 3)
choose_destination <- function(origin, agent, tt_list, n_zones = N_ZONES) {
  dest_utils <- sapply(1:n_zones, function(j) {
    if (j == origin) return(-Inf)
    mu  <- mode_utility(origin, j, agent, tt_list)
    emu <- compute_logsum(mu)
    log(zone_attr[j]) + 0.6 * emu
  })
  exp_u <- exp(dest_utils - max(dest_utils[is.finite(dest_utils)]))
  exp_u[!is.finite(exp_u)] <- 0
  probs <- exp_u / sum(exp_u)
  sample(1:n_zones, 1, prob = probs)
}

# Mode choice given fixed OD
choose_mode <- function(origin, dest, agent, tt_list) {
  mu    <- mode_utility(origin, dest, agent, tt_list)
  valid <- mu[is.finite(mu)]
  exp_u <- exp(valid - max(valid))
  probs <- exp_u / sum(exp_u)
  sample(names(valid), 1, prob = probs)
}

# Travel time lookup (minutes -> hours)
get_travel_time <- function(origin, dest, mode, tt_list) {
  tt_min <- switch(mode,
                   walk         = tt_list$walk[origin, dest],
                   bicycle      = tt_list$bicycle[origin, dest],
                   pt           = tt_list$pt[origin, dest],
                   car_driver   = tt_list$car[origin, dest],
                   car_passenger= tt_list$car_pass[origin, dest],
                   30
  )
  tt_min / 60
}

# =============================================================================
# 7. ACTIVITY DURATION SAMPLING (Figure 2, Section 2.4)
# =============================================================================

sample_duration <- function(activity_type, agent) {
  if (activity_type == "Work") {
    params <- switch(agent$employment,
                     full_time    = c(mu=8.5, sd=1.5),
                     part_time_hi = c(mu=6.0, sd=1.2),
                     part_time_lo = c(mu=4.0, sd=1.0),
                     c(mu=4.0, sd=1.0)
    )
    return(max(0.5, rnorm(1, params["mu"], params["sd"])))
  }
  params <- switch(activity_type,
                   Education = c(mu=5.0, sd=1.5),
                   Leisure   = c(mu=2.0, sd=1.2),
                   Shopping  = c(mu=1.0, sd=0.5),
                   Business  = c(mu=2.5, sd=1.0),
                   Accompany = c(mu=0.5, sd=0.3),
                   Other     = c(mu=1.5, sd=0.8),
                   c(mu=1.5, sd=0.8)
  )
  max(0.25, rnorm(1, params["mu"], params["sd"]))
}

# =============================================================================
# 8. BUILD AGENT PLAN (Section 2.5, Table 3)
#
#    Three-step procedure:
#      i.   Generate |D|=3 destination alternatives, |P|=10 duration alternatives
#      ii.  Filter by bud_perf and bud_travel, randomly combine until
#           ooh < bud_ooh (max N_TRIES attempts)
#      iii. If still failing after 4 budget iterations: force minimal plan
# =============================================================================

TIME_BUDGETS <- data.frame(
  iteration  = 1:4,
  bud_travel = c(12,  5,  4,  3),
  bud_perf   = c(14, 12, 11, 10),
  bud_ooh    = c(13.5, 14, 15, 16.5)
)

N_DEST_ALTS <- 3
N_DUR_ALTS  <- 10
N_TRIES     <- 20

build_agent_plan <- function(agent, tt_list) {
  
  home_zone <- sample(1:N_ZONES, 1)
  
  # ------------------------------------------------------------------
  # Step 1: Tour frequency choices (4 separate MNL models)
  # ------------------------------------------------------------------
  n_work  <- work_tour_freq(agent)
  n_edu   <- education_tour_freq(agent)
  n_prim  <- n_work + n_edu
  
  has_work_tour <- n_work > 0
  n_bus   <- business_tour_freq(agent, n_prim, has_work_tour)
  n_other <- other_tour_freq(agent, n_prim, n_bus, has_work_tour)
  n_tours <- n_prim + n_bus + n_other
  
  # ------------------------------------------------------------------
  # Step 2: Build activity list for each tour
  # ------------------------------------------------------------------
  activities <- list()
  tour_meta  <- list()   # tracks tour type per activity for type assignment
  
  # Primary tours (work)
  for (t in seq_len(n_work)) {
    is_work <- TRUE
    n_out   <- outbound_stop_freq(agent, n_tours, is_work)
    n_in    <- inbound_stop_freq(agent,  n_tours, is_work)
    subtour <- has_subtour(agent, n_tours, is_work)
    
    tour_acts <- character(0)
    tour_dirs <- character(0)
    
    if (n_out > 0) {
      new_acts <- replicate(n_out,
                            assign_activity_type(agent, "outbound", "work"))
      tour_acts <- c(tour_acts, new_acts)
      tour_dirs <- c(tour_dirs, rep("outbound", n_out))
    }
    tour_acts <- c(tour_acts, "Work")
    tour_dirs <- c(tour_dirs, "primary")
    
    if (subtour) {
      tour_acts <- c(tour_acts,
                     assign_activity_type(agent, "inbound", "work"))
      tour_dirs <- c(tour_dirs, "subtour")
    }
    if (n_in > 0) {
      new_acts <- replicate(n_in,
                            assign_activity_type(agent, "inbound", "work"))
      tour_acts <- c(tour_acts, new_acts)
      tour_dirs <- c(tour_dirs, rep("inbound", n_in))
    }
    
    activities <- c(activities, as.list(tour_acts))
    tour_meta  <- c(tour_meta,  as.list(tour_dirs))
  }
  
  # Primary tours (education)
  for (t in seq_len(n_edu)) {
    is_work <- FALSE
    n_out   <- outbound_stop_freq(agent, n_tours, is_work)
    n_in    <- inbound_stop_freq(agent,  n_tours, is_work)
    
    tour_acts <- character(0)
    tour_dirs <- character(0)
    
    if (n_out > 0) {
      new_acts <- replicate(n_out,
                            assign_activity_type(agent, "outbound", "education"))
      tour_acts <- c(tour_acts, new_acts)
      tour_dirs <- c(tour_dirs, rep("outbound", n_out))
    }
    tour_acts <- c(tour_acts, "Education")
    tour_dirs <- c(tour_dirs, "primary")
    
    if (n_in > 0) {
      new_acts <- replicate(n_in,
                            assign_activity_type(agent, "inbound", "education"))
      tour_acts <- c(tour_acts, new_acts)
      tour_dirs <- c(tour_dirs, rep("inbound", n_in))
    }
    
    activities <- c(activities, as.list(tour_acts))
    tour_meta  <- c(tour_meta,  as.list(tour_dirs))
  }
  
  # Secondary tours (business)
  for (t in seq_len(n_bus)) {
    n_stops   <- secondary_stop_freq(agent, n_tours, is_business_tour = TRUE)
    tour_acts <- c("Business",
                   replicate(n_stops,
                             assign_activity_type(agent, "inbound", "business")))
    activities <- c(activities, as.list(tour_acts))
    tour_meta  <- c(tour_meta,
                    as.list(rep("secondary_business", length(tour_acts))))
  }
  
  # Secondary tours (other)
  for (t in seq_len(n_other)) {
    n_stops   <- secondary_stop_freq(agent, n_tours, is_business_tour = FALSE)
    tour_acts <- c(assign_activity_type(agent, "inbound", "other"),
                   replicate(n_stops,
                             assign_activity_type(agent, "inbound", "other")))
    activities <- c(activities, as.list(tour_acts))
    tour_meta  <- c(tour_meta,
                    as.list(rep("secondary_other", length(tour_acts))))
  }
  
  # Fallback: at least one activity
  if (length(activities) == 0) {
    activities <- list("Leisure")
    tour_meta  <- list("secondary_other")
  }
  
  # ------------------------------------------------------------------
  # Step 3: Plan-building with time budgets (Section 2.5, Table 3)
  # ------------------------------------------------------------------
  valid_plan <- NULL
  
  for (iter in 1:4) {
    bud <- TIME_BUDGETS[iter, ]
    
    # Generate duration alternatives (|P| = N_DUR_ALTS, Eq. 6-7)
    dur_alts <- replicate(N_DUR_ALTS,
                          sapply(activities, function(a) sample_duration(a, agent)),
                          simplify = FALSE)
    
    dur_totals  <- sapply(dur_alts, sum)
    valid_durs  <- dur_alts[dur_totals < bud$bud_perf]
    if (length(valid_durs) == 0)
      valid_durs <- list(dur_alts[[which.min(dur_totals)]])
    
    # Generate destination alternatives (|D| = N_DEST_ALTS, Eq. 8-9)
    dest_alts <- replicate(N_DEST_ALTS, {
      current <- home_zone
      sapply(seq_along(activities), function(k) {
        d <- choose_destination(current, agent, tt_list)
        current <<- d
        d
      })
    }, simplify = FALSE)
    
    modes_alts <- lapply(dest_alts, function(dests) {
      current <- home_zone
      sapply(seq_along(dests), function(k) {
        m <- choose_mode(current, dests[k], agent, tt_list)
        current <<- dests[k]
        m
      })
    })
    
    tt_totals <- mapply(function(dests, modes) {
      current <- home_zone
      total   <- 0
      for (k in seq_along(dests)) {
        total   <- total + get_travel_time(current, dests[k], modes[k], tt_list)
        current <- dests[k]
      }
      total + get_travel_time(current, home_zone, modes[length(modes)], tt_list)
    }, dest_alts, modes_alts)
    
    valid_dest_idx <- which(tt_totals < bud$bud_travel)
    if (length(valid_dest_idx) == 0) valid_dest_idx <- which.min(tt_totals)
    valid_dests <- dest_alts[valid_dest_idx]
    valid_modes <- modes_alts[valid_dest_idx]
    valid_tt    <- tt_totals[valid_dest_idx]
    
    # Randomly combine until ooh < bud_ooh (Eq. 10)
    for (try in 1:N_TRIES) {
      d_idx      <- sample(length(valid_dests), 1)
      p_idx      <- sample(length(valid_durs),  1)
      tt_chosen  <- valid_tt[d_idx]
      dur_chosen <- sum(valid_durs[[p_idx]])
      ooh        <- tt_chosen + dur_chosen   # Eq. 10
      
      if (ooh < bud$bud_ooh) {
        valid_plan <- list(
          activities   = activities,
          durations    = valid_durs[[p_idx]],
          dests        = valid_dests[[d_idx]],
          modes        = valid_modes[[d_idx]],
          total_travel = tt_chosen,
          total_dur    = dur_chosen,
          ooh          = ooh,
          iteration    = iter,
          n_work       = n_work,
          n_edu        = n_edu,
          n_bus        = n_bus,
          n_other      = n_other,
          n_tours      = n_tours
        )
        break
      }
    }
    if (!is.null(valid_plan)) break
  }
  
  # Fallback: minimal plan if no valid plan found after 4 iterations
  if (is.null(valid_plan)) {
    valid_plan <- list(
      activities   = list("Leisure"),
      durations    = 2.0,
      dests        = sample(1:N_ZONES, 1),
      modes        = "walk",
      total_travel = 0.5,
      total_dur    = 2.0,
      ooh          = 2.5,
      iteration    = 99,
      n_work       = n_work,
      n_edu        = n_edu,
      n_bus        = n_bus,
      n_other      = n_other,
      n_tours      = n_tours
    )
  }
  
  valid_plan$home_zone <- home_zone
  valid_plan$n_trips   <- length(activities) * 2
  valid_plan
}

# =============================================================================
# 9. ACTIVITY START-TIME SCHEDULING (Section 2.5, Figure 3)
#    "Outward" approach: primary activity start time first,
#    secondary activities fitted around it.
# =============================================================================

clamp <- function(x, lo, hi) max(lo, min(hi, x))

start_time_dist <- function(activity_type) {
  switch(activity_type,
         Work      = clamp(rnorm(1,  8.0, 0.8),  5, 10),
         Education = clamp(rnorm(1,  8.2, 0.7),  6, 10),
         Leisure   = clamp(rnorm(1, 14.0, 3.0),  9, 22),
         Shopping  = clamp(rnorm(1, 11.0, 2.0),  8, 20),
         Business  = clamp(rnorm(1, 10.0, 2.0),  7, 18),
         Accompany = clamp(rnorm(1,  8.5, 1.5),  6, 18),
         Other     = clamp(rnorm(1, 12.0, 3.0),  8, 20),
         clamp(rnorm(1, 10.0, 2.0),  6, 22)
  )
}

schedule_plan <- function(plan) {
  acts <- unlist(plan$activities)
  if (length(acts) == 0) { plan$schedule <- list(); return(plan) }
  
  durs <- if (length(plan$durations) == 1)
    rep(plan$durations, length(acts))
  else
    plan$durations
  
  tt_per <- rep(plan$total_travel / max(length(acts), 1), length(acts))
  
  # Find primary activity index (Work or Education takes priority)
  prim_idx <- which(acts %in% c("Work","Education"))
  if (length(prim_idx) == 0) prim_idx <- 1
  
  prim_start   <- start_time_dist(acts[prim_idx[1]])
  current_time <- prim_start -
    sum(tt_per[seq_len(prim_idx[1]-1)]) -
    sum(durs[seq_len(max(0, prim_idx[1]-1))])
  current_time <- max(current_time, 5.0)   # no earlier than 5am
  
  schedule <- vector("list", length(acts))
  for (k in seq_along(acts)) {
    start_k <- current_time + if (k == 1) 0 else tt_per[k]
    end_k   <- start_k + durs[k]
    schedule[[k]] <- list(
      activity   = acts[k],
      start_h    = round(start_k, 2),
      end_h      = round(end_k,   2),
      duration_h = round(durs[k], 2)
    )
    current_time <- end_k
  }
  
  # Discard activities overflowing 24h (Section 2.5 — last resort, 0.06‰ of trips)
  plan$schedule <- Filter(function(s) s$start_h < 24, schedule)
  plan
}

# =============================================================================
# 10. RUN SIMULATION
# =============================================================================

cat(sprintf("Simulating %d agents...\n", N_AGENTS))

all_plans <- vector("list", N_AGENTS)
for (i in 1:N_AGENTS) {
  all_plans[[i]] <- schedule_plan(build_agent_plan(pop[i, ], tt))
  if (i %% 20 == 0) cat(sprintf("  Agent %d done\n", i))
}
cat("All agents simulated.\n\n")

# =============================================================================
# 11. VALIDATION SUMMARY (Table 4 analog)
# =============================================================================

compute_summary <- function(pop, plans) {
  n <- length(plans)
  
  mean_tours   <- mean(sapply(plans, function(p) p$n_tours))
  mean_trips   <- mean(sapply(plans, function(p) p$n_trips))
  mean_ooh     <- mean(sapply(plans, function(p) p$ooh))
  mean_tt      <- mean(sapply(plans, function(p) p$total_travel))
  mean_dur     <- mean(sapply(plans, function(p) p$total_dur))
  mean_work_t  <- mean(sapply(plans, function(p) p$n_work * 2))
  mean_leis_t  <- mean(sapply(plans, function(p)
    sum(unlist(p$activities) == "Leisure") * 2))
  
  cat("=== SYSTEM-WIDE VALIDATION SUMMARY (Table 4 analog) ===\n")
  cat(sprintf("N agents simulated          : %d\n",   n))
  cat(sprintf("Mean number of tours        : %.2f  [Paper: 1.49]\n", mean_tours))
  cat(sprintf("Mean number of trips        : %.2f  [Paper: 3.76]\n", mean_trips))
  cat(sprintf("Mean work trips             : %.2f  [Paper: 0.51]\n", mean_work_t))
  cat(sprintf("Mean leisure trips          : %.2f  [Paper: 0.72]\n", mean_leis_t))
  cat(sprintf("Mean travel time [h]        : %.2f  [Paper: 1.52]\n", mean_tt))
  cat(sprintf("Mean activity duration [h]  : %.2f  [Paper: 5.92]\n", mean_dur))
  cat(sprintf("Mean out-of-home time [h]   : %.2f  [Paper: 7.44]\n", mean_ooh))
  cat("========================================================\n\n")
}

compute_summary(pop, all_plans)

# =============================================================================
# 12. PLOTS
# =============================================================================

extract_schedule_df <- function(plans, pop) {
  rows <- list()
  for (i in seq_along(plans)) {
    plan <- plans[[i]]
    if (is.null(plan$schedule) || length(plan$schedule) == 0) next
    for (s in plan$schedule) {
      rows[[length(rows)+1]] <- data.frame(
        agent_id   = i,
        activity   = s$activity,
        start_h    = s$start_h,
        end_h      = min(s$end_h, 24),
        duration_h = s$duration_h,
        employment = pop$employment[i],
        pt_sub     = pop$pt_sub[i],
        mode       = if (length(plan$modes) > 0) plan$modes[1] else "unknown",
        stringsAsFactors = FALSE
      )
    }
  }
  bind_rows(rows)
}

sched_df <- extract_schedule_df(all_plans, pop)

# ---- Plot 1: Activity start-time distributions (Figure 3 analog) ----
p1 <- sched_df %>%
  filter(activity %in% c("Work","Education","Leisure","Shopping")) %>%
  ggplot(aes(x = start_h, fill = activity, colour = activity)) +
  geom_density(alpha = 0.3, linewidth = 0.8) +
  scale_x_continuous(breaks = seq(0, 24, 3), limits = c(0, 24)) +
  labs(
    title    = "Figure 3 analog: Activity start-time distributions",
    subtitle = paste0("N = ", N_AGENTS, " agents"),
    x = "Hour of the day", y = "Density",
    fill = "Activity", colour = "Activity"
  ) +
  theme_minimal(base_size = 13)
print(p1)

# ---- Plot 2: Work duration CDF by employment type (Figure 2 analog) ----
work_durs <- lapply(1:N_AGENTS, function(i) {
  plan <- all_plans[[i]]
  acts <- unlist(plan$activities)
  durs <- if (length(plan$durations) == 1)
    rep(plan$durations, length(acts))
  else plan$durations
  idx <- which(acts == "Work")
  if (length(idx) == 0) return(NULL)
  data.frame(employment = pop$employment[i],
             n_work     = length(idx),
             duration   = durs[idx[1]])
}) %>% bind_rows()

if (nrow(work_durs) > 0) {
  p2 <- work_durs %>%
    mutate(segment = paste0(employment, " (", n_work, " W-tour)")) %>%
    ggplot(aes(x = duration, colour = segment)) +
    stat_ecdf(linewidth = 0.9) +
    scale_x_continuous(limits = c(0,14), breaks = 0:14) +
    labs(
      title  = "Figure 2 analog: Work duration cumulative distributions",
      x = "Activity duration [hours]", y = "Cumulative probability",
      colour = "Segment"
    ) +
    theme_minimal(base_size = 13)
  print(p2)
}

# ---- Plot 3: Mode shares by PT subscription (Figure 4 analog) ----
mode_df <- lapply(1:N_AGENTS, function(i) {
  plan <- all_plans[[i]]
  data.frame(
    mode   = if (length(plan$modes) > 0) plan$modes else "walk",
    pt_sub = pop$pt_sub[i],
    stringsAsFactors = FALSE
  )
}) %>% bind_rows()

if (nrow(mode_df) > 0) {
  p3 <- mode_df %>%
    count(pt_sub, mode) %>%
    group_by(pt_sub) %>%
    mutate(share = n / sum(n)) %>%
    ungroup() %>%
    mutate(pt_sub = factor(pt_sub,
                           levels  = c("none","halbtax","verbund","general"),
                           labels  = c("No Sub","Halbtax-Abo","Verbund-Abo","General-Abo"))) %>%
    ggplot(aes(x = pt_sub, y = share, fill = mode)) +
    geom_col(position = "fill") +
    scale_y_continuous(labels = scales::percent) +
    labs(
      title    = "Figure 4 analog: Mode shares by PT subscription type",
      subtitle = "Share of trips by mode",
      x = "PT Subscription", y = "Share", fill = "Mode"
    ) +
    theme_minimal(base_size = 13) +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))
  print(p3)
}

# ---- Plot 4: Gantt chart — first 10 agents ----
act_colors <- c(Work="steelblue", Education="royalblue", Leisure="forestgreen",
                Shopping="orange", Business="purple", Accompany="pink",
                Other="grey60")

p4 <- sched_df %>%
  filter(agent_id <= 10) %>%
  mutate(agent_label = factor(paste("Agent", agent_id))) %>%
  ggplot(aes(xmin = start_h, xmax = end_h,
             ymin = as.numeric(agent_label) - 0.4,
             ymax = as.numeric(agent_label) + 0.4,
             fill = activity)) +
  geom_rect(colour = "white", linewidth = 0.3) +
  scale_x_continuous(breaks = seq(0,24,3), limits = c(0,24)) +
  scale_y_continuous(breaks = 1:10, labels = paste("Agent", 1:10)) +
  scale_fill_manual(values = act_colors, na.value = "grey80") +
  labs(title = "Agent day plans — first 10 agents",
       x = "Hour of day", y = "", fill = "Activity") +
  theme_minimal(base_size = 13)
print(p4)

# ---- Plot 5: Out-of-home time distribution vs paper benchmark ----
ooh_vals <- sapply(all_plans, function(p) p$ooh)

p5 <- ggplot(data.frame(ooh = ooh_vals), aes(x = ooh)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20,
                 fill = "steelblue", alpha = 0.7, colour = "white") +
  geom_density(colour = "steelblue", linewidth = 1) +
  geom_vline(xintercept = 7.44, linetype="dashed", colour="red",  linewidth=1) +
  geom_vline(xintercept = mean(ooh_vals), linetype="solid", colour="navy", linewidth=1) +
  annotate("text", x=7.44+0.15, y=Inf, vjust=1.5, hjust=0,
           label="Paper: 7.44h", colour="red",  size=3.5) +
  annotate("text", x=mean(ooh_vals)+0.15, y=Inf, vjust=3.5, hjust=0,
           label=sprintf("Simulated: %.2fh", mean(ooh_vals)),
           colour="navy", size=3.5) +
  labs(title = "Out-of-home time distribution",
       x = "Out-of-home time [hours]", y = "Density") +
  theme_minimal(base_size = 13)
print(p5)

# ---- Plot 6: Tour composition by employment type ----
tour_comp <- lapply(1:N_AGENTS, function(i) {
  plan <- all_plans[[i]]
  data.frame(
    employment = pop$employment[i],
    n_work     = plan$n_work,
    n_edu      = plan$n_edu,
    n_bus      = plan$n_bus,
    n_other    = plan$n_other
  )
}) %>% bind_rows() %>%
  group_by(employment) %>%
  summarise(across(starts_with("n_"), mean), .groups="drop") %>%
  pivot_longer(cols = starts_with("n_"),
               names_to = "tour_type", values_to = "mean_tours") %>%
  mutate(tour_type = recode(tour_type,
                            n_work="Work", n_edu="Education", n_bus="Business", n_other="Other"))

p6 <- ggplot(tour_comp, aes(x=employment, y=mean_tours, fill=tour_type)) +
  geom_col(position="stack") +
  labs(
    title    = "Mean number of tours by type and employment status",
    x = "Employment", y = "Mean number of tours", fill = "Tour type"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
print(p6)

cat("Done. Six plots produced.\n")