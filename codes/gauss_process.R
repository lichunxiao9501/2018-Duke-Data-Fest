bin = function(df, var, binwidth, origin = NULL) {
  n = nrow(df)
  
  var = as.character(substitute(var))
  x = df[[var]]
  
  if (is.null(origin)) {
    origin = min(x)
  }
  
  bin = (x - origin) %/% binwidth
  indices = unname(split(seq_len(n) - 1, bin))
  
  mid = origin + (0:(max(bin)+1)) * binwidth + binwidth/2
  
  df[["bin_mid"]] = mid[bin+1]
  
  attr(df, "indices") = indices
  attr(df, "drop") = FALSE
  attr(df, "group_sizes") = sapply(indices, length)
  attr(df, "biggest_group_size") = max(attr(df, "group_sizes"))
  attr(df, "labels") = data.frame(bin = seq_along(indices))
  attr(df, "vars") = list(quote(bin))
  class(df) = c("grouped_df", "tbl_df", "tbl", "data.frame")
  
  df
}

dist_long = function(d)
{
  d = as.matrix(d)
  d[upper.tri(d, diag = TRUE)] = NA
  
  data.frame(
    expand.grid(i=1:nrow(d), j=1:nrow(d)),
    c(d)
  ) %>%
    setNames(c("i","j","dist")) %>%
    filter(!is.na(dist))
}

emp_semivariogram = function(d, y, x, bin=FALSE, binwidth, range_max)
{
  y_col = as.character(substitute(y))
  x_col = as.character(substitute(x))
  
  d = d[[x_col]] %>%
    dist() %>% 
    dist_long() %>%
    mutate(y_i = d[[y_col]][i], y_j = d[[y_col]][j])
  
  
  if (bin)
  {
    d = d %>% bin(dist, binwidth = binwidth)
  } else {
    d = d %>% mutate(bin_mid = dist) %>% rowwise()
  }
  
  d = d %>%
    summarize(
      gamma = sum( (y_i - y_j)^2 / (2*n()) ),
      h = mean(bin_mid),
      n = n()
    )
  
  if (!missing(range_max))
    d = d %>% filter(h < range_max)
  
  d
}

nugget_cov   = function(d, sigma2) { ifelse(d==0, sigma2, 0) }
sq_exp_cov   = function(d, sigma2, l, sigma2_w=0) { sigma2 * exp(-(abs(d)*l)^2) + nugget_cov(d,sigma2_w) }
sq_exp_sv  = function(d, sigma2, l, sigma2_w) { sigma2 + sigma2_w - sq_exp_cov(d,sigma2,l) - nugget_cov(d,sigma2_w) }

post_summary = function(m, ci_width=0.95) {
  d = data_frame(
    post_mean  = apply(m, 2, mean),
    post_med   = apply(m, 2, median),
    post_lower = apply(m, 2, quantile, probs=(1-ci_width)/2),
    post_upper = apply(m, 2, quantile, probs=1 - (1-ci_width)/2)
  )
  
  if (!is.null(colnames(m)))
    d = d %>% mutate(param = colnames(m)) %>% select(param,post_mean:post_upper)
  
  d
}

cond_pred = function(x, y, x_pred, betas, sigma2, sigma2_w, l, reps=1000){
  beta0 = betas[1]
  beta1 = betas[2]
  beta2 = betas[3]
  
  mu = exp(beta0 + beta1*x + beta2*x^2)
  mu_pred = exp(beta0 + beta1*x_pred + beta2*x_pred^2)
  
  dist_o = fields::rdist(x)
  dist_p = fields::rdist(x_pred)
  dist_op = fields::rdist(x, x_pred)
  dist_po = t(dist_op)
  
  cov_o  = sq_exp_cov(dist_o,  sigma2 = sigma2, l = l, sigma2_w = sigma2_w)
  cov_p  = sq_exp_cov(dist_p,  sigma2 = sigma2, l = l, sigma2_w = sigma2_w)
  cov_op = sq_exp_cov(dist_op, sigma2 = sigma2, l = l, sigma2_w = sigma2_w)
  cov_po = sq_exp_cov(dist_po, sigma2 = sigma2, l = l, sigma2_w = sigma2_w)
  
  diag(cov_o) = diag(cov_o) + 5
  diag(cov_p) = diag(cov_p) + 5
  
  cond_cov = cov_p - cov_po %*% solve(cov_o) %*% cov_op
  cond_mu  = mu_pred + cov_po %*% solve(cov_o) %*% (y - mu)
  
  pred = cond_mu %*% matrix(1, ncol=reps) + t(chol(cond_cov)) %*% matrix(rnorm(length(x_pred)*reps), ncol=reps)
  
  pred_df = pred %>% t() %>% post_summary() %>% mutate(x=x_pred)
}


######## variogram plot

d_emp = rbind(
  jobAge_data %>% emp_semivariogram(avg_clicks, jobAgeDays, bin=TRUE, binwidth=1)  %>% mutate(binwidth="binwidth=0.05"),
  jobAge_data %>% emp_semivariogram(avg_clicks, jobAgeDays, bin=TRUE, binwidth=2) %>% mutate(binwidth="binwidth=0.075"),
  jobAge_data %>% emp_semivariogram(avg_clicks, jobAgeDays, bin=TRUE, binwidth=3)   %>% mutate(binwidth="binwidth=0.1"),
  jobAge_data %>% emp_semivariogram(avg_clicks, jobAgeDays, bin=TRUE, binwidth=5)  %>% mutate(binwidth="binwidth=0.15")
)

sigma2 = 22.5
sigma2_w = 2.5
l = sqrt(3)/25

d_emp %>%
  mutate(pred = sq_exp_sv(h, sigma2 = sigma2, l = l, sigma2_w = sigma2_w)) %>%
  ggplot(aes(x=h, y=gamma)) +
  geom_point() +
  geom_line(aes(y = pred), color = "red") +
  facet_wrap(~binwidth, nrow=2) +
  ggtitle("Variogram Plots")

######## pred
reps=1000

lm = lm(log(avg_clicks)~jobAgeDays+I(jobAgeDays^2), jobAge_data)
summary(lm)

betas = lm$coef

x = jobAge_data$jobAgeDays
y = jobAge_data$avg_clicks
x_pred = seq(0, 99, by = 1)

pred_df_emp = cond_pred(x, y, x_pred, betas, sigma2, sigma2_w, l, reps=1000)

ggplot(jobAge_data, aes(x=jobAgeDays, y=avg_clicks)) +
  geom_line() +
  geom_point() +
  geom_line(data=pred_df_emp, aes(x = x, y=post_mean), color='red', size=1) +
  geom_ribbon(data=pred_df_emp, aes(ymin=post_lower,ymax=post_upper, x=x, y=post_med), fill="red", alpha=0.1) +
  xlab("Job Age Days") + ylab("Average Clicks") + ggtitle("The Guassian Process Model of Average Clicks")