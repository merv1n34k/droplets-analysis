source("./R/droplets_analysis.R")

# TESTS ONLY

testdf <- data.frame(
  parentdir = "test1",
  filename = rep("test.csv", 9000),
  clean_name = rep(c("1_1_1_tag1", "1_1_2_tag1", "1_2_1_tag1", "1_1_1",
                     "1_1_2", "1_1_3", "2_1_1", "2_1_2", "2_1_1_tag1"), 1000),
  total.flow = rep(c(1,1,1,1,1,1,2,2,2),1000),
  phase.ratio = rep(c(1,1,2,1,1,1,1,1,1),1000),
  reps = rep(c(2,2,1,3,3,3,2,2,1),1000),
  tags = rep(c("tag1","tag1","tag1",NA,NA,NA,NA,NA,"tag1"),1000),
  diameter_um = rnorm(9000,100,2),
  param = rep(c("1x1","1x1","1x2","1x1","1x1","1x1","2x1","2x1","2x1"),1000)
)

testgroups <- list(
  all = list(dir = "test1"),
  tagged = list(tags = "tag1"),
  pr1 = list(pr = 1),
  pr2 = list(pr = 2),
  tf1 = list(tf = 1),
  tf2 = list(tf = 2),
  tf1t = list(tf = 1, tags = "tag1"),
  tf2t = list(tf = 2, tags = "tag1")
)

test <- process_runs(
  root = "test",
  groups = testgroups,
  compare = list(
    "all | tagged | pr1",
    "tf1 | tf2 | tf1t | tf2t",
    "pr1 | pr2"
  ),
  skip_plots = FALSE,
  custom_df = testdf
)