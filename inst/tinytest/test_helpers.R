
# Create a actors map and riskset
actors_map <- data.frame(
  id = 1:3,
  name = c("A", "B", "C"),
  stringsAsFactors = FALSE
)

rs <- matrix(
  c(1, 2,
    1, 3,
    2, 1,
    2, 3,
    3, 1,
    3, 2),
  ncol = 2, byrow = TRUE
)

### Test initialize_adj_mat

# 1. When 'initial' is numeric zero, expect an all-zero 3x3 matrix
res_adj0 <- remulate:::initialize_adj_mat(actors_map, initial = 0, rs = rs)
expected_adj0 <- array(0, dim = c(nrow(actors_map), nrow(actors_map)))
expect_equal(res_adj0, expected_adj0)

# 2. When 'initial' is a data.frame 
init_df <- data.frame(
  time = c(1, 2, 3),
  sender = c("A", "B", "A"),
  receiver = c("B", "A", "C"),
  stringsAsFactors = FALSE
)
res_adj_df <- remulate:::initialize_adj_mat(actors_map, initial = init_df, rs = rs)
expected_adj_df <- array(0, dim = c(nrow(actors_map), nrow(actors_map)))
# "A" (id 1) -> "B" (id 2) once; "B" (id 2) -> "A" (id 1) once; "A" (id 1) -> "C" (id 3) once.
expected_adj_df[1,2] <- 1
expected_adj_df[2,1] <- 1
expected_adj_df[1,3] <- 1
expect_equal(res_adj_df, expected_adj_df)

### Test initialize_beta_mat

# Create a params data frame with one row per dyad in rs.
params <- data.frame(
  sender   = c("A", "A", "B", "B", "C", "C"),
  receiver = c("B", "C", "A", "C", "A", "B"),
  value    = c(10, 20, 30, 40, 50, 60),
  stringsAsFactors = FALSE
)
res_beta <- remulate:::initialize_beta_mat(params, actors_map, rs)
expected_beta <- c(10, 20, 30, 40, 50, 60)
expect_equal(res_beta, expected_beta)

### Test get.density

evls <- matrix(
  c(1, 10,
    2, 20,
    1, 30),
  ncol = 2, byrow = TRUE
)
density <- remulate:::get.density(evls, actors = c("A", "B", "C"))
expect_equal(density, 1/3)

### Test initialize_exo_effects

actors_map <- data.frame(
  id = 1:3,
  name = c("A", "B", "C"),
  stringsAsFactors = FALSE
)

# Create a dyadic exogenous data frame 
attr_dyad <- data.frame(
  sender_id   = c("A", "B", "A", "C", "B", "C"),
  receiver_id = c("B", "A", "C", "A", "C", "B"),
  score       = c(1, 2, 3, 4, 5, 6),
  stringsAsFactors = FALSE
)

# Create an actor covariate data frame.
attr_actors <- data.frame(
  id    = c("A", "B", "C"),
  time  = c(1, 1, 1),
  value = c(100, 200, 300),
  stringsAsFactors = FALSE
)

attr_actors_list <- list(attr_dyad, attr_actors)

effects_list <- list(int_effects = c(28, 1))

res_exo <- remulate:::initialize_exo_effects(attr_actors_list, actors_map, effects_list)

expected_dyad <- as.matrix(data.frame(
  sender_id   = c(1, 2, 1, 3, 2, 3),
  receiver_id = c(2, 1, 3, 1, 3, 2),
  score       = c(1, 2, 3, 4, 5, 6),
  stringsAsFactors = FALSE
))

expected_actor <- as.matrix(data.frame(
  id    = c(1, 2, 3),
  time  = c(1, 1, 1),
  value = c(100, 200, 300),
  stringsAsFactors = FALSE
))

expect_equal(res_exo[[1]], expected_dyad)
expect_equal(res_exo[[2]], expected_actor)

### Tests for parseEffects functions
formula_tie <- ~ remulate::baseline() + remulate::inertia() + remulate::reciprocity()
res_tie <- remulate:::parseEffectsTie(formula_tie)
expect_true("baseline"    %in% names(res_tie$params))
expect_true("inertia"     %in% names(res_tie$params))
expect_true("reciprocity" %in% names(res_tie$params))

attr_actors2 <- data.frame(
  id    = c("A", "B", "C"),
  time  = c(1, 1, 1),
  value = c(100, 200, 300),
  stringsAsFactors = FALSE
)
formula_rate <- ~ remulate::baseline() + remulate::indegreeSender()
res_rate <- remulate:::parseEffectsRate(formula_rate)
expect_true("baseline"      %in% names(res_rate$params))
expect_true("indegreeSender"%in% names(res_rate$params))

attr_actors2 <- data.frame(
  id    = c("A", "B", "C"),
  time  = c(1, 1, 1),
  value = c(100, 200, 300),
  stringsAsFactors = FALSE
)
formula_choice <- ~ remulate::baseline() + remulate::inertia() + remulate::interact(indices = c(1,2))
res_choice <- remulate:::parseEffectsChoice(formula_choice)
expect_true("baseline" %in% names(res_choice$params))
expect_true("inertia"  %in% names(res_choice$params))
expect_true("interact" %in% names(res_choice$params))


### Test prepExoVar

# Test for a dyadic cov
attr_actors_exo_dyad <- data.frame(
  sender_id   = c("A", "B"),
  receiver_id = c("B", "C"),
  score       = c(10, 20),
  stringsAsFactors = FALSE
)
res_exo_var_dyad <- remulate:::prepExoVar("dyad", param = NA, scaling = "full", variable = "score", 
                                          attr_actors = attr_actors_exo_dyad)
expected_cov_dyad <- data.frame(
  sender_id   = c("A", "B"),
  receiver_id = c("B", "C"),
  val         = c(10, 20),
  stringsAsFactors = FALSE
)
expect_equal(res_exo_var_dyad$dyad$attribute, expected_cov_dyad)

# Test for a actor cov
attr_actors_exo_actor <- data.frame(
  id    = c("A", "B", "C"),
  time  = c(1, 1, 1),
  score = c(100, 200, 300),
  stringsAsFactors = FALSE
)
res_exo_var_actor <- remulate:::prepExoVar("exog", param = NA, scaling = "std", variable = "score",
                                           attr_actors = attr_actors_exo_actor)
expected_cov_actor <- data.frame(
  id    = c("A", "B", "C"),
  time  = c(1, 1, 1),
  val   = c(100, 200, 300),
  stringsAsFactors = FALSE
)
expect_equal(res_exo_var_actor$exog$attribute, expected_cov_actor)

### Test prepEndoVar

param_df <- data.frame(a = 1, b = 2, c = 3)
res_endo <- remulate:::prepEndoVar("send", param = param_df, scaling = "std", start = 0, end = 0)
expect_equal(res_endo$send$stat_name, "send")

### Test prepInteractVar

param_df_interact <- data.frame(a = 1, b = 2, c = 3)
res_interact <- remulate:::prepInteractVar(param = param_df_interact, effects = c(1, 2), scaling = "full")
expect_equal(res_interact$interact$stat_name, "interact")
