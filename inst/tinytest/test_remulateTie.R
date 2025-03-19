### Test 1: Basic simulation with only baseline effect
effects1 <- ~ remulate::baseline(-3)
set.seed(123)
sim1 <- remulate::remulateTie(effects1, actors = 1:5, endTime = 10, events = 5, initial = 0)
# Check that the simulation returns a data.frame with an edgelist
expect_true(is.data.frame(sim1))
expect_true(nrow(sim1) <= 5)
expect_true(all(c("time", "sender", "receiver") %in% names(sim1)))

# Check that the returned object has expected structure.
expect_true(inherits(sim1, "remulateTie"))
expect_true(is.data.frame(sim1))
expect_true(all(names(sim1) == c("time", "sender", "receiver")))
expect_true(!is.null(attr(sim1, "density")))
expect_true(!is.null(attr(sim1, "evls")))
expect_true(!is.null(attr(sim1, "statistics")))
expect_true(!is.null(attr(sim1, "actors")))

### Test 2: Simulation with more statistics
if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
attr_actors <- data.frame(
  id      = 1:5,
  endTime = rep(0, 5),
  sex     = c(0, 1, 0, 1, 0),
  age     = c(25, 30, 25, 30, 25)
)
effects2 <- ~ remulate::baseline(-3) +
  remulate::inertia(0.10, scaling = "std") +
  remulate::reciprocity(0.05) +
  remulate::itp(0.01, scaling = "none") +
  remulate::same(0.02, variable = "sex", attr_actors = attr_actors, scaling = "std") +
  remulate::send(0.01, variable = "age", attr_actors = attr_actors, scaling = "std") +
  remulate::tie(0.001) +
  remulate::interact(0.01, indices = c(2, 5), scaling = "std")
set.seed(123)
sim2 <- remulate::remulateTie(effects2, actors = 1:5, endTime = 10, events = 5, initial = 0)
expect_true(is.data.frame(sim2))
expect_true(nrow(sim2) <= 5)
# Check that the number of computed statistics (3rd dim) matches the number of effects in the effects formula
stats2 <- attr(sim2, "statistics")
expect_equal(dim(stats2)[3], 8)
}
### Test 3: Simulation with a data frame parameter
if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
N3 <- 10
rs_full <- as.matrix(expand.grid(1:N3, 1:N3))
rs_full <- rs_full[rs_full[, 1] != rs_full[, 2], ]
# Create df for parameter 
param_df <- as.data.frame(rs_full)
param_df$beta <- rep(0.05, nrow(param_df))
effects3 <- ~ remulate::baseline(param = -3) +
  remulate::inertia(param = param_df) +
  remulate::reciprocity(param = 0.1)
set.seed(123)
sim3 <- remulate::remulateTie(effects3, actors = 1:N3, endTime = 10, events = 5, initial = 0)
expect_true(is.data.frame(sim3))
stats3 <- attr(sim3, "statistics")
expect_equal(dim(stats3)[2], nrow(rs_full))
}
### Test 4: Simulation with a custom start time
effects4 <- ~ baseline(-3) + inertia(0.10) + reciprocity(-0.04)
set.seed(123)
sim4 <- remulate::remulateTie(effects4, actors = 1:5, endTime = 10, events = 5, startTime = 5, initial = 0)
# Check that the first event time is at or after the start time (5)
first_time <- sim4$time[1]
expect_true(first_time >= 5)

### Test 5: Simulation using the "decay" memory option
effects5 <- ~ baseline(-3) +
  inertia(0.10, scaling = "none") +
  reciprocity(-0.04)
set.seed(123)
sim5 <- remulate::remulateTie(effects5, actors = 1:5, endTime = 10, events = 5, initial = 0,
                     memory = "decay", memoryParam = 0.01)
expect_true(is.data.frame(sim5))
stats5 <- attr(sim5, "statistics")
expect_true(!is.null(stats5))

### Test 6: Simulation with a time varying effect function 
if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
baseline_func <- function(t) {
  if (t < 30) {
    -2
  } else {
    -5
  }
}
effects6 <- ~ remulate::baseline(param = baseline_func) +
  remulate::inertia(0.10) +
  remulate::reciprocity(0.04)
set.seed(123)
sim6 <- remulate::remulateTie(effects6, actors = 1:5, endTime = 10, events = 5)
expect_true(is.data.frame(sim6))
expect_true(nrow(sim6) <= 5)
}