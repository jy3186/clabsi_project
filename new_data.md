New Data Analysis
================
Jiayi Yang
2022-11-16

``` r
library("MatchIt")
```

import the new data

``` r
new_data = 
  read_excel("./LDA.xlsx", sheet = 1, range = cell_cols("A:K")) %>% 
  filter(flo_meas_name %in% c(  'G IP NYC LDA MIDLINE DOUBLE LUMEN',
                                'G IP NYC LDA MIDLINE SINGLE LUMEN',
                                'G LDA PULMONARY ARTERY CATHETER/SWAN GANZ',
                                'LDA CVC QUADRUPLE LUMEN',
                                'LDA CVC SINGLE LUMEN',
                                'LDA CVC TRIPLE LUMEN',
                                'LDA DUAL LUMEN PORT',
                                'LDA EXTERNAL URINARY CATHETER',
                                'LDA PICC SINGLE LUMEN',
                                'LDA PICC TRIPLE LUMEN',
                                'LDA PUMP DEVICE',
                                'NYC LDA PRESSURE INJURY',
                                'RETIRED - NYC ADT TC PICC DOUBLE LUMEN')
         ) %>% 
  mutate(
    placement_date = as.Date(PLACEMENT_INSTANT, "%y/%m/%d"),
    removal_date = as.Date(REMOVAL_INSTANT, "%y/%m/%d"),
    duration = removal_date - placement_date
  ) %>% 
  filter(removal_date != '2157-11-19')
```

    ## Warning in as.POSIXlt.POSIXct(x, tz = tz): unknown timezone '%y/%m/%d'

    ## Warning in as.POSIXlt.POSIXct(x, tz = tz): unknown timezone '%y/%m/%d'

``` r
new_data
```

    ## # A tibble: 6,754 × 14
    ##          EMPI BIRTH_DATE          sex_c PAT_ENC_CSN_ID hosp_admsn_time    
    ##         <dbl> <dttm>              <dbl>          <dbl> <dttm>             
    ##  1 1000039263 1937-04-28 00:00:00     2      115794137 2020-02-05 01:08:00
    ##  2 1000087431 1945-05-18 00:00:00     1      150005039 2021-07-02 15:08:00
    ##  3 1000087431 1945-05-18 00:00:00     1      150005039 2021-07-02 15:08:00
    ##  4 1000092404 1942-03-24 00:00:00     1      190464289 2022-05-07 10:23:00
    ##  5 1000092404 1942-03-24 00:00:00     1      190464289 2022-05-07 10:23:00
    ##  6 1000172922 1950-09-19 00:00:00     1      186868549 2022-03-31 22:09:00
    ##  7 1000178374 1987-03-12 00:00:00     2      179436960 2022-01-10 02:25:00
    ##  8 1000039263 1937-04-28 00:00:00     2      115794137 2020-02-05 01:08:00
    ##  9 1000039263 1937-04-28 00:00:00     2      115794137 2020-02-05 01:08:00
    ## 10 1000087431 1945-05-18 00:00:00     1      150005039 2021-07-02 15:08:00
    ## # … with 6,744 more rows, and 9 more variables: hosp_disch_time <dttm>,
    ## #   FLO_MEAS_ID <dbl>, PLACEMENT_INSTANT <dttm>, REMOVAL_INSTANT <dttm>,
    ## #   DESCRIPTION <chr>, flo_meas_name <chr>, placement_date <date>,
    ## #   removal_date <date>, duration <drtn>

``` r
new_data2 = new_data[!duplicated(new_data$EMPI), ] 
```

We have 6754 patients who have central lines inserted in our sample

``` r
bcp_df =
  read_excel("./CLABSI_data.xlsx", sheet = 3, range = cell_cols("A:K")) %>% 
  arrange(EMPI) %>% 
  select(EMPI, PROC_NAME, RESULT_TIME) %>% 
  filter(PROC_NAME == "BLOOD CULTURE") %>% 
  mutate(
  status = "blood_culture_positive",
  date = as.Date(RESULT_TIME, "%y/%m/%d")
)  %>% 
  distinct(EMPI, date, .keep_all = TRUE)
```

    ## Warning in as.POSIXlt.POSIXct(x, tz = tz): unknown timezone '%y/%m/%d'

``` r
bcp_df
```

    ## # A tibble: 2,546 × 5
    ##          EMPI PROC_NAME     RESULT_TIME         status                date      
    ##         <dbl> <chr>         <dttm>              <chr>                 <date>    
    ##  1 1000005895 BLOOD CULTURE 2021-03-20 09:36:00 blood_culture_positi… 2021-03-20
    ##  2 1000005895 BLOOD CULTURE 2021-05-06 09:39:00 blood_culture_positi… 2021-05-06
    ##  3 1000005895 BLOOD CULTURE 2021-05-11 08:51:00 blood_culture_positi… 2021-05-11
    ##  4 1000005895 BLOOD CULTURE 2021-05-03 12:48:00 blood_culture_positi… 2021-05-03
    ##  5 1000016630 BLOOD CULTURE 2020-11-12 15:04:00 blood_culture_positi… 2020-11-12
    ##  6 1000050994 BLOOD CULTURE 2020-03-29 11:02:00 blood_culture_positi… 2020-03-29
    ##  7 1000050994 BLOOD CULTURE 2020-03-28 10:29:00 blood_culture_positi… 2020-03-28
    ##  8 1000070882 BLOOD CULTURE 2021-07-31 12:06:00 blood_culture_positi… 2021-07-31
    ##  9 1000070882 BLOOD CULTURE 2021-08-01 14:21:00 blood_culture_positi… 2021-08-01
    ## 10 1000073127 BLOOD CULTURE 2021-07-19 11:24:00 blood_culture_positi… 2021-07-19
    ## # … with 2,536 more rows

We have 2546 patients who have blood culture positive shown in the EMR
in our sample

Next, see if result_time of BCP aligns with central line insert period.
Trying to join central_lined_df and bcp_df to see patients who are cl
inserted and bcp during their central line placement period.

``` r
join_test=
join_cl_bcp =
  left_join(new_data, bcp_df, by = c("EMPI")) %>%
  mutate(
   period_1 = interval(ymd(placement_date), ymd(removal_date)),
   cl_duration_time = difftime(ymd(removal_date), ymd(placement_date), units="days"),
    bcp_status = ifelse(date %within% period_1, 1, 0)
  ) %>%    

  select(EMPI, bcp_status, date, placement_date, removal_date, period_1, cl_duration_time) %>% 
  arrange(EMPI, desc(bcp_status))
  
join_test2 = join_test[!duplicated(join_test$EMPI), ] %>% 
  mutate(
    matching_period = date - placement_date,
    new_bcp_status = ifelse(is.na(bcp_status), 0, bcp_status)
  )

table(join_test2$new_bcp_status)
```

    ## 
    ##    0    1 
    ## 1840  595

Great, we found 595 observations of patients who have blood culture
positive within their periods of central line inserted.

The incidence for blood culture positive in central lined patients are
595/6754 = 8.8%

Now we try to match controls to cases by first full join the bcp_status
table (join_test2) with our original new_data table:

``` r
full_table = 
  full_join(new_data, join_test2, by = c("EMPI")) 
full_table2 = full_table[!duplicated(full_table$EMPI), ] 

full_table2$bcp_status[is.na(full_table2$new_bcp_status)] <- 0

table(full_table2$new_bcp_status)
```

    ## 
    ##    0    1 
    ## 1840  595

We have 595 cases and 1840 unique controls to pool from. `full_table2`
is the full table until now.

Matching by variable `matching_period` which stands for the time between
central line placement and tested blood culture positive

by MatchIt:

``` r
full_table3 = full_table2 %>% 
  mutate(
    new_matching_period = ifelse(is.na(matching_period), 0, matching_period)
  )

set.seed(1234)
match.it <- matchit(new_bcp_status ~ new_matching_period, data = full_table3, method="nearest", ratio=1)
matched_df <- summary(match.it)

knitr::kable(matched_df$nn, digits = 2, align = 'c', 
      caption = 'Table 2: Sample sizes')
```

|               | Control | Treated |
|:--------------|:-------:|:-------:|
| All (ESS)     |  1840   |   595   |
| All           |  1840   |   595   |
| Matched (ESS) |   595   |   595   |
| Matched       |   595   |   595   |
| Unmatched     |  1245   |    0    |
| Discarded     |    0    |    0    |

Table 2: Sample sizes

We have 595 cases and 595 matched controls by their matching period(time
of central line inserted to time of bcp). There are 1245 remained
unmatched, the total of the sample population is 2435.

save the matched dataset into a new dataframe named `df.match`

``` r
df.match <- match.data(match.it)[1:ncol(full_table3)]
rm(matched_df)

df.match %>% 
  mutate(
    BIRTH_DATE = as.Date(BIRTH_DATE, "%y/%m/%d"),
    sex_c = case_when(sex_c == 1 ~ "Male",
              sex_c == 2 ~ "Female") %>% 
      as.factor()
  ) 
```

    ## Warning in as.POSIXlt.POSIXct(x, tz = tz): unknown timezone '%y/%m/%d'

    ## # A tibble: 1,190 × 23
    ##          EMPI BIRTH_DATE sex_c  PAT_EN…¹ hosp_admsn_time     hosp_disch_time    
    ##         <dbl> <date>     <fct>     <dbl> <dttm>              <dttm>             
    ##  1 1000039263 1937-04-28 Female   1.16e8 2020-02-05 01:08:00 2020-04-01 00:27:00
    ##  2 1000087431 1945-05-18 Male     1.50e8 2021-07-02 15:08:00 2021-07-18 16:52:00
    ##  3 1000092404 1942-03-24 Male     1.90e8 2022-05-07 10:23:00 2022-05-21 05:15:00
    ##  4 1000172922 1950-09-19 Male     1.87e8 2022-03-31 22:09:00 2022-04-07 03:03:00
    ##  5 1000178374 1987-03-12 Female   1.79e8 2022-01-10 02:25:00 2022-01-20 18:35:00
    ##  6 1000018092 1953-10-27 Male     1.85e8 2022-03-12 19:48:00 2022-04-26 16:22:00
    ##  7 1000135489 1989-11-02 Male     1.28e8 2020-10-01 10:00:00 2020-10-08 17:09:00
    ##  8 1000016630 1980-01-06 Female   1.28e8 2020-10-30 10:40:00 2020-11-15 09:33:00
    ##  9 1000070882 1958-08-08 Female   1.52e8 2021-07-28 00:33:00 2021-08-12 19:00:00
    ## 10 1000086187 1949-01-22 Female   1.74e8 2021-11-29 16:05:00 2022-01-10 18:10:00
    ## # … with 1,180 more rows, 17 more variables: FLO_MEAS_ID <dbl>,
    ## #   PLACEMENT_INSTANT <dttm>, REMOVAL_INSTANT <dttm>, DESCRIPTION <chr>,
    ## #   flo_meas_name <chr>, placement_date.x <date>, removal_date.x <date>,
    ## #   duration <drtn>, bcp_status <dbl>, date <date>, placement_date.y <date>,
    ## #   removal_date.y <date>, period_1 <Interval>, cl_duration_time <drtn>,
    ## #   matching_period <drtn>, new_bcp_status <dbl>, new_matching_period <dbl>,
    ## #   and abbreviated variable name ¹​PAT_ENC_CSN_ID

``` r
df.match
```

    ## # A tibble: 1,190 × 23
    ##          EMPI BIRTH_DATE          sex_c PAT_ENC_CSN_ID hosp_admsn_time    
    ##         <dbl> <dttm>              <dbl>          <dbl> <dttm>             
    ##  1 1000039263 1937-04-28 00:00:00     2      115794137 2020-02-05 01:08:00
    ##  2 1000087431 1945-05-18 00:00:00     1      150005039 2021-07-02 15:08:00
    ##  3 1000092404 1942-03-24 00:00:00     1      190464289 2022-05-07 10:23:00
    ##  4 1000172922 1950-09-19 00:00:00     1      186868549 2022-03-31 22:09:00
    ##  5 1000178374 1987-03-12 00:00:00     2      179436960 2022-01-10 02:25:00
    ##  6 1000018092 1953-10-27 00:00:00     1      185024635 2022-03-12 19:48:00
    ##  7 1000135489 1989-11-02 00:00:00     1      127547323 2020-10-01 10:00:00
    ##  8 1000016630 1980-01-06 00:00:00     2      128010373 2020-10-30 10:40:00
    ##  9 1000070882 1958-08-08 00:00:00     2      151803384 2021-07-28 00:33:00
    ## 10 1000086187 1949-01-22 00:00:00     2      174308016 2021-11-29 16:05:00
    ## # … with 1,180 more rows, and 18 more variables: hosp_disch_time <dttm>,
    ## #   FLO_MEAS_ID <dbl>, PLACEMENT_INSTANT <dttm>, REMOVAL_INSTANT <dttm>,
    ## #   DESCRIPTION <chr>, flo_meas_name <chr>, placement_date.x <date>,
    ## #   removal_date.x <date>, duration <drtn>, bcp_status <dbl>, date <date>,
    ## #   placement_date.y <date>, removal_date.y <date>, period_1 <Interval>,
    ## #   cl_duration_time <drtn>, matching_period <drtn>, new_bcp_status <dbl>,
    ## #   new_matching_period <dbl>

df.match has 1190 observations include 595 cases and 595 controls

demographic analysis: age, gender

``` r
library(lemon)
```

    ## 
    ## Attaching package: 'lemon'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     %||%

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     element_render

``` r
   age <- function(BIRTH_DATE, on.day=today()) {
    intvl <- interval(BIRTH_DATE, on.day)
    prd <- as.period(intvl)
    return(prd@year)
   }
pop = sample(x = 1:100, size = 20)
ggplot(df.match, aes(x = ifelse(test = sex_c == "Male", yes = -pop, no = pop), y = BIRTH_DATE, fill = sex_c)) +
  geom_col() +
  scale_x_symmetric(labels = abs) +
  labs(x = "Population") 
```

![](new_data_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

Look at the exposure TPN status Next, we look at TPN data

``` r
tpn_df =
   read_excel("./CLABSI_data.xlsx", sheet = 4, range = cell_cols("A:K")) %>% 
  arrange(EMPI) %>% 
  select(EMPI, START_DATE, END_DATE) %>% 
  mutate(
    status = "tpn"
  )
join_tpn = 
  left_join(df.match, tpn_df, by = c("EMPI")) %>% 
  mutate(
    tpn_status = ifelse(START_DATE %within% period_1, 1, 0)
  ) %>% 
    arrange(EMPI, desc(tpn_status)) %>% 
  select(EMPI, new_bcp_status, period_1, START_DATE, END_DATE, tpn_status, duration) %>% 
  mutate(
    new_tpn_status = ifelse(is.na(tpn_status), 0, tpn_status)
  ) %>% 
    distinct(EMPI, .keep_all = TRUE) 

tpn_table = join_tpn%>% 
    janitor::tabyl(new_tpn_status, new_bcp_status)
tpn_table
```

    ##  new_tpn_status   0   1
    ##               0 575 577
    ##               1  20  18

This is the 2\*2 table of exposure on the left, outcome on the right. OR
= ad/bc = 0.8969

## Conditional logistic regression

Possible covariates: age, gender, hospitalization duration, central line
placement duration…

First, using `mylogit` to do a logistic regression on the outcome
variable (having blood culture positive) and on the predictor variable
(TPN status). We also want to look at potential confounder so we
included duration as a covariate.

``` r
mylogit <- glm(new_bcp_status ~ new_tpn_status + duration, data = join_tpn, family = "binomial")
```

``` r
summary(mylogit)
```

    ## 
    ## Call:
    ## glm(formula = new_bcp_status ~ new_tpn_status + duration, family = "binomial", 
    ##     data = join_tpn)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.5486  -1.1290   0.0226   1.2038   1.3283  
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)    -0.168134   0.070909  -2.371   0.0177 *  
    ## new_tpn_status -0.193226   0.337581  -0.572   0.5671    
    ## duration        0.013292   0.003233   4.112 3.93e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1626.1  on 1172  degrees of freedom
    ## Residual deviance: 1597.0  on 1170  degrees of freedom
    ##   (17 observations deleted due to missingness)
    ## AIC: 1603
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
confint.default(mylogit)
```

    ##                     2.5 %      97.5 %
    ## (Intercept)    -0.3071134 -0.02915445
    ## new_tpn_status -0.8548736  0.46842077
    ## duration        0.0069560  0.01962898

Test result discussion: For the predictor `new_tpn_status`, p-value
0.5671 is greater than 0.05 which indicates that it is not statistically
significant. Our model suggests that having total parental nutrition
does not significantly impact the result of blood culture positive. The
Odds Ratio of having TPN to BCP is eβ = e^-0.1932 = 0.8243. This
indicates that: the total parental nutrition group has 0.8243 times the
odds of non total parental nutrition group of having blood culture
positive. (?This makes TPN become a protective factor?)

For the predictor `duration`, p-value 3.93e-05 is smaller than 0.05
which indicates that it is statistically significant. Our model suggests
that duration of central lines placement does significantly impact the
result of blood culture positive. The Odds Ratio of central line
placement duration to BCP is eβ = e^0.0133 = 1.013. This indicates that:
An increase of 1 day in central line placement period is associated with
an increase of 1.3% in the odds of blood culture positive.

------------------------------------------------------------------------

Wald test? For hypothesis testing and assumption check? Not sure about
the assumption part.
