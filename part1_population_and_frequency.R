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
business_tour_freq <- function(agent, n_primary, has_work_tour) {
  e <- emp_dummies(agent); pt <- pt_dummies(agent)
  age_n <- agent$age / 80; acc_h <- agent$accessibility_home
  V0 <- 0
  V1 <- ( -0.50 + 0.90*e$d_fulltime + 0.60*e$d_parttime_hi + 0.30*e$d_parttime_lo
          - 0.40*age_n - 0.10*agent$children_in_hh + 0.30*agent$car_avail
          + 0.10*pt$d_halbtax + 0.15*pt$d_verbund + 0.20*pt$d_general
          - 0.50*n_primary + 0.40*has_work_tour + 0.20*acc_h )
  V2 <- ( -2.00 + 0.60*e$d_fulltime + 0.30*e$d_parttime_hi + 0.10*e$d_parttime_lo
          - 0.60*age_n - 0.20*agent$children_in_hh + 0.40*agent$car_avail
          + 0.05*pt$d_halbtax + 0.10*pt$d_verbund + 0.15*pt$d_general
          - 0.80*n_primary + 0.50*has_work_tour + 0.15*acc_h )
  as.integer(mnl_choice(c("0"=V0, "1"=V1, "2"=V2)))
}

# --- 3d. Other tour frequency (0/1/2) ---
other_tour_freq <- function(agent, n_primary, n_business, has_work_tour) {
  e <- emp_dummies(agent); pt <- pt_dummies(agent)
  age_n <- agent$age / 80; dist <- agent$dist_primary_km / 50
  acc_h <- agent$accessibility_home
  n_tours_so_far <- n_primary + n_business
  V0 <- 0
  V1 <- (  0.20 + 0.20*e$d_fulltime + 0.30*e$d_parttime_hi + 0.40*e$d_parttime_lo
          - 0.20*age_n + 0.30*agent$children_in_hh + 0.25*agent$car_avail
          + 0.10*pt$d_halbtax + 0.20*pt$d_verbund + 0.25*pt$d_general
          - 0.30*dist - 0.40*n_tours_so_far - 0.30*n_primary
          - 0.20*has_work_tour + 0.25*acc_h )
  V2 <- ( -1.00 + 0.10*e$d_fulltime + 0.20*e$d_parttime_hi + 0.30*e$d_parttime_lo
          - 0.40*age_n + 0.40*agent$children_in_hh + 0.30*agent$car_avail
          + 0.05*pt$d_halbtax + 0.15*pt$d_verbund + 0.20*pt$d_general
          - 0.50*dist - 0.70*n_tours_so_far - 0.50*n_primary
          - 0.30*has_work_tour + 0.20*acc_h )
  as.integer(mnl_choice(c("0"=V0, "1"=V1, "2"=V2)))
}

# =============================================================================
# 4. STOP FREQUENCY — 4 separate MNL models (Table 1, Section 2.4)
# =============================================================================

# --- 4a. Outbound stop frequency (0/1/2) ---
outbound_stop_freq <- function(agent, n_tours, is_work_tour) {
  e <- emp_dummies(agent); pt <- pt_dummies(agent)
  age_n <- agent$age / 80; dist <- agent$dist_primary_km / 50
  acc_h <- agent$accessibility_home; acc_p <- agent$accessibility_primary
  V0 <- 0
  V1 <- ( -0.50 + 0.20*e$d_fulltime + 0.40*e$d_parttime_hi + 0.50*e$d_parttime_lo
          - 0.30*age_n + 0.60*agent$children_in_hh + 0.25*agent$car_avail
          + 0.10*pt$d_halbtax + 0.15*pt$d_verbund + 0.20*pt$d_general
          - 0.40*dist - 0.20*n_tours - 0.20*is_work_tour + 0.15*acc_h + 0.10*acc_p )
  V2 <- ( -2.00 + 0.10*e$d_fulltime + 0.20*e$d_parttime_hi + 0.30*e$d_parttime_lo
          - 0.50*age_n + 0.80*agent$children_in_hh + 0.30*agent$car_avail
          + 0.05*pt$d_halbtax + 0.10*pt$d_verbund + 0.15*pt$d_general
          - 0.60*dist - 0.40*n_tours - 0.30*is_work_tour + 0.10*acc_h + 0.10*acc_p )
  as.integer(mnl_choice(c("0"=V0, "1"=V1, "2"=V2)))
}

# --- 4b. Inbound stop frequency (0/1/2) ---
inbound_stop_freq <- function(agent, n_tours, is_work_tour) {
  e <- emp_dummies(agent); pt <- pt_dummies(agent)
  age_n <- agent$age / 80; dist <- agent$dist_primary_km / 50
  acc_h <- agent$accessibility_home; acc_p <- agent$accessibility_primary
  V0 <- 0
  V1 <- ( -0.20 + 0.30*e$d_fulltime + 0.45*e$d_parttime_hi + 0.55*e$d_parttime_lo
          - 0.30*age_n + 0.50*agent$children_in_hh + 0.30*agent$car_avail
          + 0.10*pt$d_halbtax + 0.20*pt$d_verbund + 0.25*pt$d_general
          - 0.35*dist - 0.20*n_tours + 0.20*is_work_tour + 0.20*acc_h + 0.15*acc_p )
  V2 <- ( -1.80 + 0.20*e$d_fulltime + 0.30*e$d_parttime_hi + 0.40*e$d_parttime_lo
          - 0.50*age_n + 0.60*agent$children_in_hh + 0.35*agent$car_avail
          + 0.05*pt$d_halbtax + 0.15*pt$d_verbund + 0.20*pt$d_general
          - 0.55*dist - 0.40*n_tours + 0.25*is_work_tour + 0.15*acc_h + 0.10*acc_p )
  as.integer(mnl_choice(c("0"=V0, "1"=V1, "2"=V2)))
}

# --- 4c. Secondary tour stop frequency (0/1/2) ---
secondary_stop_freq <- function(agent, n_tours, is_business_tour) {
  e <- emp_dummies(agent); pt <- pt_dummies(agent)
  age_n <- agent$age / 80; acc_h <- agent$accessibility_home
  V0 <- 0
  V1 <- (  0.00 + 0.10*e$d_fulltime + 0.20*e$d_parttime_hi + 0.30*e$d_parttime_lo
          - 0.20*age_n + 0.30*agent$children_in_hh + 0.25*agent$car_avail
          + 0.10*pt$d_halbtax + 0.15*pt$d_verbund + 0.20*pt$d_general
          - 0.30*n_tours - 0.30*is_business_tour + 0.20*acc_h )
  V2 <- ( -1.50 + 0.05*e$d_fulltime + 0.15*e$d_parttime_hi + 0.25*e$d_parttime_lo
          - 0.40*age_n + 0.40*agent$children_in_hh + 0.30*agent$car_avail
          + 0.05*pt$d_halbtax + 0.10*pt$d_verbund + 0.15*pt$d_general
          - 0.60*n_tours - 0.50*is_business_tour + 0.15*acc_h )
  as.integer(mnl_choice(c("0"=V0, "1"=V1, "2"=V2)))
}

# --- 4d. Subtour (work -> X -> work) ---
# Hard constraint: subtour requires a primary work tour (Section 2.4)
has_subtour <- function(agent, n_tours, is_work_tour) {
  if (!is_work_tour) return(FALSE)
  e <- emp_dummies(agent); pt <- pt_dummies(agent)
  age_n <- agent$age / 80; dist <- agent$dist_primary_km / 50
  acc_h <- agent$accessibility_home; acc_p <- agent$accessibility_primary
  V_no  <- 0
  V_yes <- ( -2.00 + 0.50*e$d_fulltime + 0.30*e$d_parttime_hi + 0.10*e$d_parttime_lo
             - 0.40*age_n - 0.30*agent$children_in_hh + 0.30*agent$car_avail
             + 0.10*pt$d_halbtax + 0.15*pt$d_verbund + 0.20*pt$d_general
             - 0.50*dist - 0.30*n_tours + 0.40*is_work_tour
             + 0.20*acc_h + 0.30*acc_p )
  mnl_choice(c("no"=V_no, "yes"=V_yes)) == "yes"
}

# =============================================================================
# 5. ACTIVITY TYPE ASSIGNMENT (Section 2.4)
#    Segmented by person group, direction and tour type.
#    Probabilities approximated — exact tables not published in paper.
# =============================================================================

assign_activity_type <- function(agent, direction = "inbound", tour_type = "other") {
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
  } else if (direction == "subtour") {
      # Mid-day excursion from primary work location
      # Leisure and shopping dominate (lunch, errands near workplace)
      # Probabilities do not vary by employment since subtours are
      # by definition only made by workers
      probs <- c(Leisure  = 0.40,
                 Shopping = 0.30,
                 Business = 0.15,
                 Education= 0.00,
                 Accompany= 0.05,
                 Other    = 0.10)
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

agent_test$secondary_stop_freq <- sapply(1:N_TEST, function(i)
  secondary_stop_freq(agent_test[i, ],
                      agent_test_n_tours[i],
                      agent_test_is_business_tour[i]))

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
    if (subtour)   acts <- c(acts, assign_activity_type(agent, "inbound", "work"))
    if (n_in > 0)  acts <- c(acts, replicate(n_in,  assign_activity_type(agent, "inbound",  "work")))
    acts <- as.character(unlist(acts))
    tours[[length(tours)+1]] <- list(type="work", activities=acts)
  }

  for (t in seq_len(n_edu)) {
    n_out <- outbound_stop_freq(agent, n_tours, is_work_tour = FALSE)
    n_in  <- inbound_stop_freq(agent,  n_tours, is_work_tour = FALSE)
    acts  <- character(0)
    if (n_out > 0) acts <- c(acts, replicate(n_out, assign_activity_type(agent, "outbound", "education")))
    acts <- c(acts, "Education")
    if (n_in > 0)  acts <- c(acts, replicate(n_in,  assign_activity_type(agent, "inbound",  "education")))
    acts <- as.character(unlist(acts))
    tours[[length(tours)+1]] <- list(type="education", activities=acts)
  }

  for (t in seq_len(n_bus)) {
    n_stops <- secondary_stop_freq(agent, n_tours, is_business_tour = TRUE)
    acts    <- c("Business", replicate(n_stops, assign_activity_type(agent, "inbound", "business")))
    acts <- as.character(unlist(acts))
    tours[[length(tours)+1]] <- list(type="business", activities=acts)
  }

  for (t in seq_len(n_other)) {
    n_stops <- secondary_stop_freq(agent, n_tours, is_business_tour = FALSE)
    acts    <- c(assign_activity_type(agent, "inbound", "other"),
                 replicate(n_stops, assign_activity_type(agent, "inbound", "other")))
    acts <- as.character(unlist(acts))
    tours[[length(tours)+1]] <- list(type="other", activities=acts)
  }

  if (length(tours) == 0)
    tours <- list(list(type="other", activities="Leisure"))

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
