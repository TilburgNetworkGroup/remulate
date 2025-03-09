effects <- ~ remulate::baseline(-3) + remulate::inertia(0.10, scaling = "std")

# --- Test 2: Memory parameter error ---
# When memory is "window" (or "window_m") but no memoryParam is provided, it should stop.
expect_error(
  remulate::remulateTie(effects, actors = 1:5, endTime = 10, events = 5, memory = "window"),
  "Cannot use window memory technique without a memoryParam value"
)

# --- Test 3: Initial edgelist with event time after simulation time ---
# Create an initial data.frame with one event time later than the simulation time.
initial_df <- data.frame(endTime = c(5, 12), sender = c(1, 2), receiver = c(2, 3))
expect_error(
  remulate::remulateTie(effects, actors = 1:5, endTime = 10, events = 5, initial = initial_df),
  "Last event of initial data.frame is after 'endTime' argument"
)

# --- Test 4: Riskset with actor not in the actor list ---
# Create a custom risk set matrix using actor names as characters,
# and include actor ("6") that is not in the supplied actors argumetn.
riskset_bad <- matrix(c("6", "2", "1", "3"), ncol = 2)
expect_error(
  remulate::remulateTie(effects, actors = as.character(1:5), endTime = 10, events = 5, riskset = riskset_bad),
  "risk set contains sender actor not specified in actor's list"
)
