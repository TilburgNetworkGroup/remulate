### Test 1: Basic simulation with only baseline effect
rateform <- ~ remulate::baseline(-3)
choiceform <- ~ remulate::baseline(-3)
set.seed(123)
sim1 <- remulate::remulateActor(rateform, choiceform, actors = 1:5, endTime = 10, events = 5, initial = 0)
# Check that the simulation returns a data.frame with an edgelist
expect_true(is.data.frame(sim1))
expect_true(nrow(sim1) <= 5)

# Check that the returned object has expected structure.
expect_true(inherits(sim1, "remulateActor"))
expect_true(is.data.frame(sim1))
expect_true(!is.null(attr(sim1, "density")))
expect_true(!is.null(attr(sim1, "evls")))
expect_true(!is.null(attr(sim1, "rateStatistics")))
expect_true(!is.null(attr(sim1, "choiceStatistics")))
expect_true(!is.null(attr(sim1, "actors")))
expect_true(!is.null(attr(sim1, "riskset")))


### Test 2: Simulation with more statistics
cov <- data.frame(
  id      = 1:5,
  endTime = rep(0, 5),
  sex     = c(0, 1, 0, 1, 0),
  age     = c(25, 30, 25, 30, 25)
)
rateform <- ~ remulate::baseline(-3) +
  remulate::outdegreeSender(0.2)+
  remulate::send(0.1, variable = "age", attr_actors = cov)
choiceform <- ~ remulate::baseline(-3) +
  remulate::reciprocity(0.05) +
  remulate::tie(0.001) +
  remulate::same(0.02, variable = "sex", attr_actors = cov) +
  remulate::interact(0.01, indices = c(2, 3), scaling = "std")
set.seed(123)
sim2 <- remulate::remulateActor(rateform, choiceform, actors = 1:5, endTime = 10, events = 5)
expect_true(is.data.frame(sim2))
expect_true(nrow(sim2) <= 5)
rateStats <- attr(sim2, "rateStatistics")
choiceStats <- attr(sim2, "choiceStatistics")
expect_equal(dim(rateStats)[3], 3)
expect_equal(dim(choiceStats)[3], 5)

### Test 4: Simulation with a custom start time
rateform <- ~ remulate::baseline(-3) +  remulate::outdegreeSender(0.2)
choiceform <- ~ remulate::reciprocity(0.4)
set.seed(123)
sim4 <- remulate::remulateActor(rateform, choiceform, actors = 1:5, endTime = 100, events = 5, startTime = 5)
# Check that the first event time is at or after the start time (5)
first_time <- sim4$time[1]
expect_true(first_time >= 5)

### Test 5: Simulation using the "decay" memory option
rateform <- ~ remulate::baseline(-3) 
choiceform <- ~ remulate::reciprocity(-0.04)
set.seed(123)
sim5 <- remulate::remulateActor(rateform, choiceform, actors = 1:5, endTime = 10, events = 5, memory = "decay", memoryParam = 0.01)
expect_true(is.data.frame(sim5))
rateStats5 <- attr(sim5, "rateStatistics")
choiceStats5 <- attr(sim5, "choiceStatistics")
expect_true(!is.null(rateStats5))
expect_true(!is.null(choiceStats5))

### Test 6: Simulation with a time varying effect function
baseline_func <- function(t) {
  if(t < 30) {
    -2
  } else {
    -5
  }
}
rateform <- ~ remulate::baseline(param = baseline_func)
choiceform <- ~ remulate::reciprocity(0.04)
set.seed(123)
sim6 <- remulate::remulateActor(rateform, choiceform, actors = 1:5, endTime = 10, events = 5, initial = 0)
expect_true(is.data.frame(sim6))
expect_true(nrow(sim6) <= 5)

