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