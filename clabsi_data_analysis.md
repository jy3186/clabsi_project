clabsi_data_analysis
================
Jiayi Yang
2022-10-04

Importing and tidying CLABSI data, keeping duplicative appeared patients
only once

``` r
central_lined_df =
  read_excel("./CLABSI_data.xlsx", sheet = 1, range = cell_cols("A:G")) %>% 
  arrange(EMPI) %>% 
  select(EMPI, ordering_date) %>% 
mutate(
  status = "central_lined",
  end_date = ymd(ordering_date) + days(14)
) %>% 
  distinct(EMPI, ordering_date, .keep_all = TRUE) %>% 
  mutate(
      period_1 = interval(ymd(ordering_date), ymd(end_date)),
      date = ordering_date
  )

central_lined_df
```

    ## # A tibble: 6,744 × 6
    ##        EMPI ordering_date       status end_date   period_1                      
    ##       <dbl> <dttm>              <chr>  <date>     <Interval>                    
    ##  1   1.00e9 2021-04-15 00:00:00 centr… 2021-04-29 2021-04-15 UTC--2021-04-29 UTC
    ##  2   1.00e9 2021-05-01 00:00:00 centr… 2021-05-15 2021-05-01 UTC--2021-05-15 UTC
    ##  3   1.00e9 2021-04-17 00:00:00 centr… 2021-05-01 2021-04-17 UTC--2021-05-01 UTC
    ##  4   1.00e9 2021-05-10 00:00:00 centr… 2021-05-24 2021-05-10 UTC--2021-05-24 UTC
    ##  5   1.00e9 2020-11-07 00:00:00 centr… 2020-11-21 2020-11-07 UTC--2020-11-21 UTC
    ##  6   1.00e9 2020-11-06 00:00:00 centr… 2020-11-20 2020-11-06 UTC--2020-11-20 UTC
    ##  7   1.00e9 2022-03-17 00:00:00 centr… 2022-03-31 2022-03-17 UTC--2022-03-31 UTC
    ##  8   1.00e9 2021-01-17 00:00:00 centr… 2021-01-31 2021-01-17 UTC--2021-01-31 UTC
    ##  9   1.00e9 2021-04-18 00:00:00 centr… 2021-05-02 2021-04-18 UTC--2021-05-02 UTC
    ## 10   1.00e9 2020-03-08 00:00:00 centr… 2020-03-22 2020-03-08 UTC--2020-03-22 UTC
    ## # … with 6,734 more rows, and 1 more variable: date <dttm>

Bloodstream Infectious importing and cleaning

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

See if result_time of BCP aligns with central line insert period.

Trying to join central_lined_df and bcp_df to see patients who are cl
inserted and bcp

``` r
join_cl_bcp =
  left_join(central_lined_df, bcp_df, by = c("EMPI")) %>% 
  mutate(
    join_status = ifelse(date.y %within% period_1, 1, 0)
  ) %>% 
   filter(join_status == 1)

join_cl_bcp
```

    ## # A tibble: 1,743 × 11
    ##       EMPI ordering_date       statu…¹ end_date   period_1                      
    ##      <dbl> <dttm>              <chr>   <date>     <Interval>                    
    ##  1  1.00e9 2021-05-01 00:00:00 centra… 2021-05-15 2021-05-01 UTC--2021-05-15 UTC
    ##  2  1.00e9 2021-05-01 00:00:00 centra… 2021-05-15 2021-05-01 UTC--2021-05-15 UTC
    ##  3  1.00e9 2021-05-01 00:00:00 centra… 2021-05-15 2021-05-01 UTC--2021-05-15 UTC
    ##  4  1.00e9 2020-11-07 00:00:00 centra… 2020-11-21 2020-11-07 UTC--2020-11-21 UTC
    ##  5  1.00e9 2020-11-06 00:00:00 centra… 2020-11-20 2020-11-06 UTC--2020-11-20 UTC
    ##  6  1.00e9 2020-03-24 00:00:00 centra… 2020-04-07 2020-03-24 UTC--2020-04-07 UTC
    ##  7  1.00e9 2020-03-24 00:00:00 centra… 2020-04-07 2020-03-24 UTC--2020-04-07 UTC
    ##  8  1.00e9 2021-07-28 00:00:00 centra… 2021-08-11 2021-07-28 UTC--2021-08-11 UTC
    ##  9  1.00e9 2021-07-28 00:00:00 centra… 2021-08-11 2021-07-28 UTC--2021-08-11 UTC
    ## 10  1.00e9 2022-02-09 00:00:00 centra… 2022-02-23 2022-02-09 UTC--2022-02-23 UTC
    ## # … with 1,733 more rows, 6 more variables: date.x <dttm>, PROC_NAME <chr>,
    ## #   RESULT_TIME <dttm>, status.y <chr>, date.y <date>, join_status <dbl>, and
    ## #   abbreviated variable name ¹​status.x

Next, we look at TPN data

``` r
tpn_df =
   read_excel("./CLABSI_data.xlsx", sheet = 4, range = cell_cols("A:K")) %>% 
  arrange(EMPI) %>% 
  select(EMPI, START_DATE, END_DATE) %>% 
  mutate(
    status = "tpn"
  )
tpn_df
```

    ## # A tibble: 2,310 × 4
    ##          EMPI START_DATE          END_DATE            status
    ##         <dbl> <dttm>              <dttm>              <chr> 
    ##  1 1000039263 2020-03-24 00:00:00 2020-03-25 00:00:00 tpn   
    ##  2 1000039263 2020-03-25 00:00:00 2020-03-26 00:00:00 tpn   
    ##  3 1000039263 2020-03-26 00:00:00 2020-03-27 00:00:00 tpn   
    ##  4 1000039263 2020-03-27 00:00:00 2020-03-28 00:00:00 tpn   
    ##  5 1000039263 2020-03-28 00:00:00 2020-03-29 00:00:00 tpn   
    ##  6 1000391782 2021-07-24 00:00:00 2021-07-25 00:00:00 tpn   
    ##  7 1000391782 2021-07-25 00:00:00 2021-07-26 00:00:00 tpn   
    ##  8 1000391782 2021-07-29 00:00:00 2021-07-30 00:00:00 tpn   
    ##  9 1000708322 2020-06-05 00:00:00 2020-06-06 00:00:00 tpn   
    ## 10 1000708322 2020-06-06 00:00:00 2020-06-07 00:00:00 tpn   
    ## # … with 2,300 more rows

## R Markdown