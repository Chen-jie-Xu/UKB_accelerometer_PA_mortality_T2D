X <- c("light_PA_min", "moderate_PA_min", "vigorous_PA_min", "MVPA_min")
Y <- c("death_HES", "death_cancer", "death_CVD")
covs <- c("age_accel", "sex", "new_ethnic", "qualification","season_wear", 
          "duration_wear", "smoke", "alcohol_unit", "diet_score", "sleep_score1",
          "diabetes_duration", "BMI", "wasit", "history_cancer_CVD", "self_hypertension", 
          "health_self", "long_illness_injury", "illness_injury_2years")

xtitle <- c("LPA (minutes/week)","MPA (minutes/week)","VPA (minutes/week)","MVPA (minutes/week)")
ytitle <- c("all-cause mortality","cancer mortality","cardiovascular disease mortality")

# loop part
for(j in 1:length(Y)){
  for(i in 1:length(X)){
    x <- X[i]
    y <- Y[j]
    xt <- xtitle[i]
    yt <- ytitle[j]
    kn <- 4
    
    indf <- dplyr::select(baseline, all_of("follow_death"), all_of(covs), all_of(y), all_of(x)) %>%
      dplyr::rename("y"=y,"x"=x,"time"="follow_death")
    
    indf <- as.data.frame(lapply(indf, as.numeric))
    
    lim1 <- quantile(indf$x,probs = c(0.01, 0.99))[1]
    lim99 <- quantile(indf$x,probs = c(0.01, 0.99))[2]
    
    dd <- NULL
    dd <<- rms::datadist(indf)
    options(datadist='dd')
    formula <- paste0("survival::Surv(time,y) ~ rms::rcs(x,", kn, ")", " + ", paste0(covs, collapse = " + "))
    model <- rms::cph(as.formula(formula), data = indf, x = T, y = T, surv = T)
    pvalue_all <- anova(model)[1,3]
    pvalue_nonlin <- round(anova(model)[2,3], 3)
    pre.model <- rms::Predict(model, x, fun = exp, 
                              type = "predictions", ref.zero = T, conf.int = 0.95, digits = 2)
    
    refvalue <- lim1
    dd <- rms::datadist(indf)
    dd[["limits"]]["Adjust to", "x"] <- refvalue
    old <- options()
    on.exit(options(old))
    options(datadist = "dd")
    model <- update(model)
    model.logistic <- model
    pre.model <- rms::Predict(model.logistic, x, fun = exp, 
                              type = "predictions", ref.zero = T, conf.int = 0.95, digits = 2)
    
    df <- as.data.frame(dplyr::select(pre.model, x, yhat, lower, upper))
    
    assign(paste0("df",i,"_",j),df)
    
    colnames(df) <- c("x", "y", "lower", "upper")
    
    label1 <- paste0("P[Overall]", ifelse(pvalue_all < 0.001, 
                                          "<0.001", paste0("==",sprintf("%.3f", pvalue_all))))
    label2 <- paste0("P[Nonlinear]",ifelse(pvalue_nonlin < 0.001, 
                                           "<0.001", paste0("==",sprintf("%.3f", pvalue_nonlin))))
    
    yt <- paste0("HR(95% CI) for ",yt)
    
  }
}
