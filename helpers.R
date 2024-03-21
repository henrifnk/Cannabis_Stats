simulate_mixed_ordinal = function(pers, beta, intercepts, p_eff) {
  df <- data.frame(
    age = rep(round(runif(pers, 18, 65)), each = rep_measures),
    education = rep(as.factor(sample(c("High_School", "Some_College", "Other_College"),
                                     pers, replace = TRUE)), each = rep_measures),
    gender = rep(sample(c("male", "female"), pers, replace = TRUE), each = rep_measures),
    treatment = rep(c(0, 1), pers * rep_measures / 2),
    setup = rep(rep(c("Canabis", "Alcohol", "Stealth", "Cocain", "Robbery"),
                    each = 2), pers * rep_measures / 10),
    p_id = rep(1:pers, each = rep_measures)
  )   # control potential outcome
  df$setup = factor(df$setup, levels = c("Canabis", "Alcohol", "Stealth", "Cocain", "Robbery"))
  df$education = factor(df$education, levels = c("High_School", "Some_College", "Other_College"))
  df$gender = factor(df$gender, levels = c("male", "female"))
  df_matrix = model.matrix(~ age + education + gender + treatment * setup - 1,
                           data = df)
  pers_eff = rep(p_eff, each = rep_measures) * 0.5
  log_odds_raw = df_matrix %*% beta + pers_eff
  cum_probs <- lapply(intercepts, function(beta0){
    log_odds = beta0 + log_odds_raw
    exp(log_odds) / (1 + exp(log_odds))
  })
  
  cum_probs = do.call("cbind", cum_probs)
  cum_probs = cbind(1, cum_probs)
  probs = matrix(NA, nrow = nrow(cum_probs), ncol = ncol(cum_probs))
  for (i in 1:(ncol(cum_probs) - 1)) {
    probs[, i] = cum_probs[, i] - cum_probs[, i + 1]
  }
  probs[, ncol(cum_probs)] = cum_probs[, ncol(cum_probs)]
  df$Y = apply(probs, 1, function(x) {
    sample(1:ncol(cum_probs), 1, prob = x)
  })
  df$Y = ordered(df$Y, levels = 1:8,
                 labels = (c("Kein Strafbedürfnis",
                             "monatliches Nettogehalt",
                             "2x monatliches Nettogehalt",
                             "3x monatliches Nettogehalt",
                             "Freiheitsstrafe bis 2 Jahre mit Bewährung",
                             "Freiheitsstrafe bis 2 Jahre ohne Bewährung",
                             "Freiheitsstrafe 2 - 5 Jahre",
                             "Freiheitsstrafe über 5 Jahre")))
  df
}