# -----------Generate Input Data-----------
# Create the data frame
painting_data <- data.frame(
  schemaID = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
  payoff = c(3, 4, 5, 2, 5, 6, 6, 2, 4, 3, 4, 5, 2, 3, 6),
  author = c("Dali", "Ciurlionis", "Velazquez", "Hokusai", "VanGogh", "Botticelli", 
             "Munkacsy", "Picasso", "DaVinci", "Chagall", "Monet", "Munch", 
             "OKeeffe", "Michelangelo", "Goya"),
  familirarity = c(0.388316151, 0.04467354, 0.079037801, 0.079037801, 0.615120275, 
                   0.219931271, 0.020618557, 0.560137457, 0.536082474, 0.085910653, 
                   0.463917526, 0.195876289, 0.072164948, 0.384879725, 0.161512027),
  new = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1),
  type = c("same", "same", "same", "replaced", "replaced", "replaced", "same", 
           "same", "replaced", "replaced", "new", "new", "new", "new", "new"),
  mean0 = c(24.40043609, 26.22740557, 38.11269084, 12.30300888, 29.885084, 
            47.54525908, 50.91984497, 22.92546818, 29.58977965, 21.77354907, 
            21.04766581, 26.4393834, 18.27657794, 19.82444596, 49.52130893),
  modvar = c(5.181766043, 11.11918442, 9.358035718, 4.169477774, 10.26733847, 
             18.83542809, 22.17450084, 17.18497774, 10.61967136, 3.720593243, 
             12.20478566, 10.82212773, 14.90293958, 5.696560695, 32.19337558)
)

# Create data directory if it doesn't exist
if (!dir.exists("data")) {
  dir.create("data")
}

# Write to CSV file
write.csv(painting_data, "data/painting_schemainfo2.csv", row.names = FALSE)





# -----------Simulation Running-----------
source('src/model_adaptive_strategies.R')

typelist <- c("H", "Hc", "HL", "L", "Lc", "LH")
exp_typelist <- c("painting", "quote")

subjectnum <- 100
Param.df_baseline_1500s <- data.frame(
  Subject = 1:subjectnum,
  a_schema = rep(0.2, subjectnum),
  h_schema = rep(1000, subjectnum),
  Beta_N = rep(0.2, subjectnum),
  Beta_Var = rep(0.3, subjectnum),
  a_generic = rep(0.1, subjectnum),
  h_generic = rep(1500, subjectnum),
  Beta_gN = rep(0.1, subjectnum),
  Beta_gVar = rep(0.2, subjectnum),
  w = rep(0.3, subjectnum),
  Phi = rep(20, subjectnum),
  decay_speed = rep(0.999, subjectnum),
  decay_speed_thres = rep(0.999, subjectnum),
  thres_item_inter = rep(6, subjectnum),
  thres_item_final = rep(13.75, subjectnum),
  thres_schema = rep(50, subjectnum),
  theta_shift = rep(3, subjectnum),
  timevar = rep(0.0001, subjectnum),
  modeltimestep = rep(0.061, subjectnum),
  # 新增参数
  gamma_incentive = rep(0, subjectnum),
  gamma_inhibition = rep(0, subjectnum),
  WSLS_boost_factor = rep(0, subjectnum),
  WSLS_penalty_factor = rep(0, subjectnum),
  thres_con = rep(0.3, subjectnum),
  AC_threshold = rep(0, subjectnum),
  give_up_schema_thres = rep(0, subjectnum),
  give_up_item_final_thres = rep(7.5, subjectnum),
  payoff_attention_weight = rep(0.5, subjectnum),
  before_time = rep(1500, subjectnum)
)



Param.df_feedback <- Param.df_baseline_1500s
Param.df_feedback$gamma_incentive <- rep(0.3, subjectnum)
Param.df_feedback$gamma_inhibition <- rep(0.2, subjectnum)

Param.df_depression <- Param.df_baseline_1500s
Param.df_depression$gamma_incentive <- rep(0.01, subjectnum)
Param.df_depression$gamma_inhibition <- rep(0.5, subjectnum)
Param.df_depression$thres_con <- rep(0.6, subjectnum)

Param.df_excitement <- Param.df_baseline_1500s
Param.df_excitement$gamma_incentive <- rep(0.6, subjectnum)
Param.df_excitement$gamma_inhibition <- rep(0.05, subjectnum)
Param.df_excitement$thres_con <- rep(0.1, subjectnum)



Param.df_payoff_1500s <- Param.df_baseline_1500s
Param.df_payoff_1500s$AC_threshold <- rep(2, subjectnum)

Param.df_WSLS_1500s <- Param.df_baseline_1500s
Param.df_WSLS_1500s$WSLS_boost_factor <- rep(100, subjectnum)
Param.df_WSLS_1500s$WSLS_penalty_factor <- rep(0.01, subjectnum)

Param.df_integration_1500s <- Param.df_baseline_1500s
Param.df_integration_1500s$AC_threshold <- rep(0.25, subjectnum)
Param.df_integration_1500s$WSLS_boost_factor <- rep(100, subjectnum)
Param.df_integration_1500s$WSLS_penalty_factor <- rep(0.01, subjectnum)



Param.df_baseline_2500s <- Param.df_baseline_1500s
Param.df_baseline_2500s$before_time <- rep(2500, subjectnum)

Param.df_payoff_2500s <- Param.df_baseline_2500s
Param.df_payoff_2500s$AC_threshold <- rep(2, subjectnum)
Param.df_payoff_2500s$before_time <- rep(2500, subjectnum)

Param.df_WSLS_2500s <- Param.df_baseline_1500s
Param.df_WSLS_2500s$WSLS_boost_factor <- rep(100, subjectnum)
Param.df_WSLS_2500s$WSLS_penalty_factor <- rep(0.01, subjectnum)
Param.df_WSLS_2500s$before_time <- rep(2500, subjectnum)

Param.df_integration_2500s <- Param.df_baseline_1500s
Param.df_integration_2500s$AC_threshold <- rep(0.25, subjectnum)
Param.df_integration_2500s$WSLS_boost_factor <- rep(100, subjectnum)
Param.df_integration_2500s$WSLS_penalty_factor <- rep(0.01, subjectnum)
Param.df_integration_2500s$before_time <- rep(2500, subjectnum)



res <- simulation(Param.df_feedback, "L", "painting", save = TRUE,
                  savepath = "res_feedback/", sim.mode = "before",
                  save.confi = TRUE, scale.confi.init = TRUE)

res <- simulation(Param.df_depression, "L", "painting", save = TRUE,
                  savepath = "res_depression/", sim.mode = "before",
                  save.confi = TRUE, scale.confi.init = TRUE)

res <- simulation(Param.df_excitement, "L", "painting", save = TRUE,
                  savepath = "res_excitement/", sim.mode = "before",
                  save.confi = TRUE, scale.confi.init = TRUE)



res <- simulation(Param.df_baseline_1500s, "L", "painting", save = TRUE,
                  savepath = "res_baseline_1500s/", sim.mode = "before",
                  save.confi = TRUE, scale.confi.init = TRUE)

res <- simulation(Param.df_payoff_1500s, "L", "painting", save = TRUE,
                  savepath = "res_payoff_1500s/", sim.mode = "before",
                  save.confi = TRUE, scale.confi.init = TRUE)

res <- simulation(Param.df_WSLS_1500s, "L", "painting", save = TRUE,
                  savepath = "res_WSLS_1500s/", sim.mode = "before",
                  save.confi = TRUE, scale.confi.init = TRUE)

res <- simulation(Param.df_integration_1500s, "L", "painting", save = TRUE,
                  savepath = "res_integration_1500s/", sim.mode = "before",
                  save.confi = TRUE, scale.confi.init = TRUE)



res <- simulation(Param.df_baseline_2500s, "L", "painting", save = TRUE,
                  savepath = "res_baseline_2500s/", sim.mode = "before",
                  save.confi = TRUE, scale.confi.init = TRUE)

res <- simulation(Param.df_payoff_2500s, "L", "painting", save = TRUE,
                  savepath = "res_payoff_2500s/", sim.mode = "before",
                  save.confi = TRUE, scale.confi.init = TRUE)

res <- simulation(Param.df_WSLS_2500s, "L", "painting", save = TRUE,
                  savepath = "res_WSLS_2500s/", sim.mode = "before",
                  save.confi = TRUE, scale.confi.init = TRUE)

res <- simulation(Param.df_integration_2500s, "L", "painting", save = TRUE,
                  savepath = "res_integration_2500s/", sim.mode = "before",
                  save.confi = TRUE, scale.confi.init = TRUE)
