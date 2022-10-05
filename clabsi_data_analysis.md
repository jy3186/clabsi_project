clabsi_data_analysis
================
Jiayi Yang
2022-10-04

Importing and tidying CLABSI data

``` r
central_lined_df =
  read_excel("./CLABSI_data.xlsx", sheet = 1, range = cell_cols("A:G")) %>% 
  arrange(EMPI) %>% 
  select(EMPI, ordering_date) %>% 
mutate(
  status = "central_lined"
)
central_lined_df
```

    ## # A tibble: 6,978 × 3
    ##          EMPI ordering_date       status       
    ##         <dbl> <dttm>              <chr>        
    ##  1 1000005895 2021-04-15 00:00:00 central_lined
    ##  2 1000005895 2021-05-01 00:00:00 central_lined
    ##  3 1000005895 2021-04-17 00:00:00 central_lined
    ##  4 1000012361 2021-05-10 00:00:00 central_lined
    ##  5 1000016630 2020-11-07 00:00:00 central_lined
    ##  6 1000016630 2020-11-06 00:00:00 central_lined
    ##  7 1000018092 2022-03-17 00:00:00 central_lined
    ##  8 1000027309 2021-01-17 00:00:00 central_lined
    ##  9 1000030188 2021-04-18 00:00:00 central_lined
    ## 10 1000039263 2020-03-08 00:00:00 central_lined
    ## # … with 6,968 more rows

Bloodstream Infectious

``` r
bcp_df =
  read_excel("./CLABSI_data.xlsx", sheet = 3, range = cell_cols("A:J")) %>% 
  arrange(EMPI) %>% 
  select(EMPI, PROC_NAME, RESULT_TIME) %>% 
  filter(PROC_NAME == "BLOOD CULTURE") %>% 
mutate(
  status = "blood_culture_positive"
)
bcp_df
```

    ## # A tibble: 4,752 × 4
    ##          EMPI PROC_NAME     RESULT_TIME         status                
    ##         <dbl> <chr>         <dttm>              <chr>                 
    ##  1 1000005895 BLOOD CULTURE 2021-03-20 09:36:00 blood_culture_positive
    ##  2 1000005895 BLOOD CULTURE 2021-05-06 09:39:00 blood_culture_positive
    ##  3 1000005895 BLOOD CULTURE 2021-05-11 08:51:00 blood_culture_positive
    ##  4 1000005895 BLOOD CULTURE 2021-05-11 08:52:00 blood_culture_positive
    ##  5 1000005895 BLOOD CULTURE 2021-03-20 09:36:00 blood_culture_positive
    ##  6 1000005895 BLOOD CULTURE 2021-03-20 09:36:00 blood_culture_positive
    ##  7 1000005895 BLOOD CULTURE 2021-05-03 12:48:00 blood_culture_positive
    ##  8 1000005895 BLOOD CULTURE 2021-05-06 09:36:00 blood_culture_positive
    ##  9 1000005895 BLOOD CULTURE 2021-05-06 09:39:00 blood_culture_positive
    ## 10 1000005895 BLOOD CULTURE 2021-05-11 08:50:00 blood_culture_positive
    ## # … with 4,742 more rows

TPN data

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

Trying to join central_lined_df and tpn_df to see patients who are cl
inserted and tpn

``` r
join_cl_tpn =
  left_join(central_lined_df, tpn_df, by = "EMPI") %>% 
  drop_na() %>% 
   filter(!is.na("status"))
join_cl_tpn
```

    ## # A tibble: 4,591 × 6
    ##         EMPI ordering_date       statu…¹ START_DATE          END_DATE           
    ##        <dbl> <dttm>              <chr>   <dttm>              <dttm>             
    ##  1    1.00e9 2020-03-08 00:00:00 centra… 2020-03-24 00:00:00 2020-03-25 00:00:00
    ##  2    1.00e9 2020-03-08 00:00:00 centra… 2020-03-25 00:00:00 2020-03-26 00:00:00
    ##  3    1.00e9 2020-03-08 00:00:00 centra… 2020-03-26 00:00:00 2020-03-27 00:00:00
    ##  4    1.00e9 2020-03-08 00:00:00 centra… 2020-03-27 00:00:00 2020-03-28 00:00:00
    ##  5    1.00e9 2020-03-08 00:00:00 centra… 2020-03-28 00:00:00 2020-03-29 00:00:00
    ##  6    1.00e9 2020-03-08 00:00:00 centra… 2020-03-24 00:00:00 2020-03-25 00:00:00
    ##  7    1.00e9 2020-03-08 00:00:00 centra… 2020-03-25 00:00:00 2020-03-26 00:00:00
    ##  8    1.00e9 2020-03-08 00:00:00 centra… 2020-03-26 00:00:00 2020-03-27 00:00:00
    ##  9    1.00e9 2020-03-08 00:00:00 centra… 2020-03-27 00:00:00 2020-03-28 00:00:00
    ## 10    1.00e9 2020-03-08 00:00:00 centra… 2020-03-28 00:00:00 2020-03-29 00:00:00
    ## # … with 4,581 more rows, 1 more variable: status.y <chr>, and abbreviated
    ## #   variable name ¹​status.x

## R Markdown
