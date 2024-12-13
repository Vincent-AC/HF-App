#' HollowFibre1CompAbsParam
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
HollowFibre1CompAbsParam <- function() {
  calc_ke <- function(half_life){
    log(2)/half_life
  }

  calc_t_peak <- function(ka,ke){
    1/(ka-ke) * log(ka/ke)
  }

  calc_dose <- function(c_peak,f_avail,ka,v,ke,t_peak){
    c_peak * (1/((f_avail*ka/(v*(ka-ke))) * (exp(-ke*t_peak)-exp(-ka*t_peak))))
  }

  calc_f_dose <- function(ka,t_n){
    1-exp(-ka*t_n)
  }

  calc_a_i <- function(dose,ka,t_i){
    dose * exp(-ka*t_i)
  }

  calc_t_i_rounded <- function(t_i){
    round(t_i*60,0)
  }

  calc_t_i_interval_rounded <- function(t_i_minus_1,t_i){
    round((t_i*60-t_i_minus_1*60),0)
  }

  calc_s_i_rounded <- function(a_i_minus_1,a_i,t_i_interval_rounded){
    (a_i_minus_1-a_i)/(t_i_interval_rounded/60)
  }

  calc_v_inf_rounded <- function(s_i_rounded,c_j,t_i_rounded){
    (s_i_rounded/60/c_j)*t_i_rounded*1000
  }

  calc_v_inf_decimal_rounded <- function(v_inf_rounded){
    v_inf_rounded %% 1
  }

  calc_max_v_inf_decimal_rounded <- function(n,ka,f_dose,dose,c)
  {
    max_v_inf_decimal_rounded <- tibble(t_i = c(0,calc_ti(1:12,n,ka,f_dose)),
                                        a_i = calc_a_i(dose,ka,t_i),
                                        s_i = calc_s_i(lag(a_i),a_i,lag(t_i),t_i),
                                        th_amt_transferred = dose * exp(-ka*lag(t_i))-dose * exp(-ka*t_i),
                                        amt_transferred_s_i = s_i*(t_i-lag(t_i)),
                                        t_i_interval_rounded = calc_t_i_interval_rounded(lag(t_i),t_i),
                                        s_i_rounded = calc_s_i_rounded(lag(a_i),a_i,t_i_interval_rounded),
                                        v_inf_rounded = calc_v_inf_rounded(s_i_rounded,c,t_i_interval_rounded),
                                        v_inf_decimal_rounded = calc_v_inf_decimal_rounded(v_inf_rounded)) %>%
      slice(-1) %>%
      pull(v_inf_decimal_rounded) %>%
      max()

    tibble(c,max_v_inf_decimal_rounded)
  }

  calc_ti <- function(i,n,ka,f_dose){
    -(log(1-f_dose*(i/n)))/ka
  }

  calc_s_i <- function(a_i_minus_1,
                       a_i,
                       t_i_minus_1,
                       t_i){
    (a_i_minus_1-a_i)/(t_i-t_i_minus_1)
  }

  library(tidyverse)
  library(knitr)

  n = 12
  t_n = 12
  c_peak = 4.25
  ka = 0.36
  half_life = 3.15
  v = 0.360
  f_avail = 1
  c_list = seq(1,1000,1)

  ke <- calc_ke(half_life)
  t_peak <- calc_t_peak(ka,ke)
  dose <- calc_dose(c_peak,f_avail,ka,v,ke,t_peak)
  f_dose <- calc_f_dose(ka,t_n)

  optimal_v_inf_decimal_rounded <- map_df(c_list,~calc_max_v_inf_decimal_rounded(n,ka,f_dose,dose,.x)) %>%
    filter(max_v_inf_decimal_rounded==min(max_v_inf_decimal_rounded))

  c_optimal <- optimal_v_inf_decimal_rounded %>% pull(c)

  final_table_optimal <- tibble(t_i = c(0,calc_ti(1:12,n,ka,f_dose)),
                                a_i = calc_a_i(dose,ka,t_i),
                                s_i = calc_s_i(lag(a_i),a_i,lag(t_i),t_i),
                                th_amt_transferred = dose * exp(-ka*lag(t_i))-dose * exp(-ka*t_i),
                                amt_transferred_s_i = s_i*(t_i-lag(t_i)),
                                t_i_rounded = calc_t_i_rounded(t_i),
                                t_i_interval_rounded = calc_t_i_interval_rounded(lag(t_i),t_i),
                                s_i_rounded = calc_s_i_rounded(lag(a_i),a_i,t_i_interval_rounded),
                                v_inf_rounded = calc_v_inf_rounded(s_i_rounded,c_optimal,t_i_interval_rounded),
                                v_inf_decimal_rounded = calc_v_inf_decimal_rounded(v_inf_rounded))


  final_table_optimal %>%
    mutate_all(function(x)
      round(x, 2)) %>%
    kable(
      col.names = c(
        "$t_{i}$ (h)",
        "$A_{depot}$ (mg)",
        "$S_{i}$ (mg/h)",
        "$A_{transferred,target,i}$ (mg)",
        "$A_{transferred,slope,i}$ (mg)",
        "$t_{i,rounded}$ (min)",
        "$t_{i,interval,rounded}$ (min)",
        "$S_{i,rounded}$ (mg/h)",
        "$V_{i,inf,rounded}$ (mL)",
        "$V_{i,inf,decimal,rounded}$ (mL)"
      ),
      escape = FALSE
    )
}
