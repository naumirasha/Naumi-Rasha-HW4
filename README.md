# Naumi-Rasha-HW4
Lab3
NAUMI RASHA
2024-03-01
R Markdown
This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see http://rmarkdown.rstudio.com.

When you click the Knit button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

summary(cars)
##      speed           dist       
##  Min.   : 4.0   Min.   :  2.00  
##  1st Qu.:12.0   1st Qu.: 26.00  
##  Median :15.0   Median : 36.00  
##  Mean   :15.4   Mean   : 42.98  
##  3rd Qu.:19.0   3rd Qu.: 56.00  
##  Max.   :25.0   Max.   :120.00
Including Plots
You can also embed plots, for example:



Note that the echo = FALSE parameter was added to the code chunk to prevent printing of the R code that generated the plot.

library(ggplot2)
library(tidyverse)
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.1.4     ✔ readr     2.1.5
## ✔ forcats   1.0.0     ✔ stringr   1.5.1
## ✔ lubridate 1.9.3     ✔ tibble    3.2.1
## ✔ purrr     1.0.2     ✔ tidyr     1.3.1
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
load("~/Documents/brfss22/BRFSS2022/BRFSS2022_rev.RData")
brfss22$Age_midpt <- fct_recode(brfss22$X_AGEG5YR, "21" = "Age 18 to 24",
                                "27" = "Age 25 to 29", "32" = "Age 30 to 34",
                                "37" = "Age 35 to 39", "42" = "Age 40 to 44",
                                "47" = "Age 45 to 49", "52" = "Age 50 to 54",
                                "57" = "Age 55 to 59", "62" = "Age 60 to 64",
                                "67" = "Age 65 to 69", "72" = "Age 70 to 74",
                                "77" = "Age 75 to 79", "82" = "Age 80 or older",
                                NULL = "Dont know/Refused/Missing")
brfss22$Age_midpt <- as.numeric(levels(brfss22$Age_midpt))[brfss22$Age_midpt]
select_tristate <- (brfss22$X_STATE == "New York") | (brfss22$X_STATE == "New Jersey") | (brfss22$X_STATE == "Connecticut")
brfss_tristate <- subset(brfss22,select_tristate)
p_tri <- ggplot(data = brfss_tristate,
                mapping = aes(x = Age_midpt,
                              y = X_BMI5))
p_tri + geom_smooth()
## `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
## Warning: Removed 6288 rows containing non-finite values (`stat_smooth()`).


p_tri + geom_jitter(width = 2.5, height = NULL, alpha = 0.05)
## Warning: Removed 6288 rows containing missing values (`geom_point()`).


p_tri <- ggplot(data = brfss_tristate,
                mapping = aes(x = Age_midpt,
                              y = X_BMI5,
                              color = X_STATE,
                              fill = X_STATE))
p_tri + geom_smooth()
## `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
## Warning: Removed 6288 rows containing non-finite values (`stat_smooth()`).


ACEdidntask <- (as.numeric(is.na(brfss22$ACEDEPRS)) + 
                        as.numeric(is.na(brfss22$ACEDRINK)) +
                        as.numeric(is.na(brfss22$ACEDRUGS)) +
                        as.numeric(is.na(brfss22$ACEPRISN)) +
                        as.numeric(is.na(brfss22$ACEDIVRC)) +
                        as.numeric(is.na(brfss22$ACEPUNCH)) +
                        as.numeric(is.na(brfss22$ACEHURT1)) +
                        as.numeric(is.na(brfss22$ACESWEAR)) +
                        as.numeric(is.na(brfss22$ACETOUCH)) )
select_ACE <- (ACEdidntask == 0) # with zero missing values for any of ACE questions
brfss_ACE <- subset(brfss22, select_ACE)
xtabs(~ brfss_ACE$ACEDRINK + brfss_ACE$ACEDRUGS)
##                                                                                          brfss_ACE$ACEDRUGS
## brfss_ACE$ACEDRINK                                                                        Yes, Adverse Childhood Exper, lived with someone who used illegal street drugs or who abused prescription medications
##   Yes, Adverse Childhood Exper, lived with someone who was a problem drinker or alcoholic                                                                                                                  3164
##   No                                                                                                                                                                                                       1483
##   dont know not sure                                                                                                                                                                                         13
##   refused                                                                                                                                                                                                     4
##                                                                                          brfss_ACE$ACEDRUGS
## brfss_ACE$ACEDRINK                                                                           No
##   Yes, Adverse Childhood Exper, lived with someone who was a problem drinker or alcoholic  8157
##   No                                                                                      33919
##   dont know not sure                                                                        107
##   refused                                                                                    64
##                                                                                          brfss_ACE$ACEDRUGS
## brfss_ACE$ACEDRINK                                                                        dont know not sure
##   Yes, Adverse Childhood Exper, lived with someone who was a problem drinker or alcoholic                140
##   No                                                                                                      83
##   dont know not sure                                                                                      77
##   refused                                                                                                  5
##                                                                                          brfss_ACE$ACEDRUGS
## brfss_ACE$ACEDRINK                                                                        refused
##   Yes, Adverse Childhood Exper, lived with someone who was a problem drinker or alcoholic      11
##   No                                                                                           31
##   dont know not sure                                                                            4
##   refused                                                                                     710
ftable(xtabs(~ brfss_ACE$ACEDRINK + brfss_ACE$ACEDRUGS))
##                                                                                         brfss_ACE$ACEDRUGS Yes, Adverse Childhood Exper, lived with someone who used illegal street drugs or who abused prescription medications    No dont know not sure refused
## brfss_ACE$ACEDRINK                                                                                                                                                                                                                                               
## Yes, Adverse Childhood Exper, lived with someone who was a problem drinker or alcoholic                                                                                                                                     3164  8157                140      11
## No                                                                                                                                                                                                                          1483 33919                 83      31
## dont know not sure                                                                                                                                                                                                            13   107                 77       4
## refused                                                                                                                                                                                                                        4    64                  5     710
brfss_ACE$ACEDRINK_recode <- fct_recode(brfss_ACE$ACEDRINK, 
                                        " " = "Yes, Adverse Childhood Exper, lived with someone who was a problem drinker or alcoholic",
                                " " = "No", 
                                " " = "dont know not sure",
                                " " = "refused"
)

# might want to set some values as missing,
#                                 NULL = "dont know not sure"
ACEdidntask <- (as.numeric(is.na(brfss22$ACEDEPRS)) + 
                        as.numeric(is.na(brfss22$ACEDRINK)) +
                        as.numeric(is.na(brfss22$ACEDRUGS)) +
                        as.numeric(is.na(brfss22$ACEPRISN)) +
                        as.numeric(is.na(brfss22$ACEDIVRC)) +
                        as.numeric(is.na(brfss22$ACEPUNCH)) +
                        as.numeric(is.na(brfss22$ACEHURT1)) +
                        as.numeric(is.na(brfss22$ACESWEAR)) +
                        as.numeric(is.na(brfss22$ACETOUCH)) )
select_ACE <- (ACEdidntask == 0) # with zero missing values for any of ACE questions
summary(ACEdidntask)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   9.000   9.000   8.027   9.000   9.000
quantile(ACEdidntask, probs = c(0.01,0.05,0.1,0.15,0.2))
##  1%  5% 10% 15% 20% 
##   0   0   0   9   9
summary(brfss22)
##        X_STATE                          X_METSTAT               X_URBSTAT     
##  Washington: 26152   Metropolitan counties   :318082   urban counties:380732  
##  New York  : 17800   nonmetropolitan counties:117642   rural counties: 54992  
##  Minnesota : 16821   NA's                    :  9408   NA's          :  9408  
##  Ohio      : 16487                                                            
##  Maryland  : 16418                                                            
##  Texas     : 14245                                                            
##  (Other)   :337209                                                            
##                                MSCODE          CHILDREN    
##  in central city                  : 29393   Min.   : 0.00  
##  in county containing central city: 16030   1st Qu.: 0.00  
##  in suburb near city              : 16068   Median : 0.00  
##  outside MSA                      : 32395   Mean   : 0.49  
##  NA's                             :351246   3rd Qu.: 1.00  
##                                             Max.   :82.00  
##                                             NA's   :14464  
##                             MARITAL      
##  Married                        :227424  
##  Never married                  : 80001  
##  Divorced                       : 57516  
##  Widowed                        : 48019  
##  A member of an unmarried couple: 18668  
##  (Other)                        : 13496  
##  NA's                           :     8  
##                                                           EDUCA       
##  College 4 years or more (College graduate)                  :187496  
##  College 1 year to 3 years (Some college or technical school):120252  
##  Grade 12 or GED (High school graduate)                      :108990  
##  Grades 9 through 11 (Some high school)                      : 16954  
##  Grades 1 through 8 (Elementary)                             :  8381  
##  (Other)                                                     :  3054  
##  NA's                                                        :     5  
##           VETERAN3                                   X_PRACE2     
##  Yes a veteran: 53211   White                            :351032  
##  No           :386272   Black or African American        : 41522  
##  NA's         :  5649   Asian                            : 14836  
##                         American Indian or Alaskan Native: 10147  
##                         Refused                          :  8593  
##                         (Other)                          : 18989  
##                         NA's                             :    13  
##                      X_HISPANC                X_AGEG5YR          RENTHOM1     
##  yes Hispanic             : 42917   Age 65 to 69   : 47099   own home:310708  
##  no                       :396631   Age 60 to 64   : 44511   rent    :108332  
##  dont know refused missing:  5584   Age 70 to 74   : 43472   other   : 21463  
##                                     Age 55 to 59   : 36821   NA's    :  4629  
##                                     Age 80 or older: 36251                    
##                                     Age 50 to 54   : 33644                    
##                                     (Other)        :203334                    
##                EMPLOY1      
##  Employed for wages:186004  
##  Retired           :137083  
##  Self-employed     : 38768  
##  Unable to work    : 26737  
##  A homemaker       : 17477  
##  (Other)           : 27823  
##  NA's              : 11240  
##                                                 INCOME3      
##  Less than $75,000 ($50,000 to less than $75,000)   : 59148  
##  Less than $150,000 ($100,000 to less than $150,000): 50330  
##  Less than $100,000 ($75,000 to less than $100,000) : 48436  
##  Refused                                            : 47001  
##  Less than $50,000 ($35,000 to less than $50,000)   : 46831  
##  (Other)                                            :180454  
##  NA's                                               : 12932  
##                  FOODSTMP     
##  got food stamps SNAP: 25323  
##  no                  :226638  
##  NA's                :193171  
##                               
##                               
##                               
##                               
##                                                                                               SDHFOOD1     
##  Never                                                                                            :203654  
##  Rarely                                                                                           : 20768  
##  Sometimes                                                                                        : 17258  
##  Always the food that you bought not last, and you didn\x92t have money to get more, in last 12 mo:  4824  
##  Usually                                                                                          :  4603  
##  (Other)                                                                                          :  1722  
##  NA's                                                                                             :192303  
##     SEXVAR                      BIRTHSEX     
##  Male  :209239   male sex at birth  : 37441  
##  Female:235893   female sex at birth: 41456  
##                  NA's               :366235  
##                                              
##                                              
##                                              
##                                              
##                         SOMALE                             SOFEMALE     
##  Gay                       :  2939   Lesbian or Gay            :  2318  
##  Straight, that is, not gay:112679   Straight, that is, not gay:123813  
##  Bisexual                  :  2570   Bisexual                  :  5507  
##  Something else            :  1974   Something else            :  2789  
##  I dont know the answer    :  1057   I dont know the answer    :  1760  
##  Refused                   :  2813   Refused                   :  3443  
##  NA's                      :321100   NA's                      :305502  
##                                    TRNSGNDR     
##  Yes, Transgender, male-to-female      :   499  
##  Yes, Transgender, female to male      :   515  
##  Yes, Transgender, gender nonconforming:   589  
##  No                                    :258106  
##  Dont know/Not Sure                    :   811  
##  Refused                               :  3041  
##  NA's                                  :181571  
##                       HADSEX            GENHLTH          PHYSHLTH     
##  yes had sex in last 6 mo: 14744   Very good:148444   Min.   : 0.000  
##  no                      :  4231   Good     :143598   1st Qu.: 0.000  
##  dont know not sure      :    66   Excellent: 71878   Median : 0.000  
##  refused                 :   835   Fair     : 60273   Mean   : 4.348  
##  NA's                    :425256   Poor     : 19741   3rd Qu.: 3.000  
##                                    (Other)  :  1195   Max.   :30.000  
##                                    NA's     :     3   NA's   :10927   
##     MENTHLTH                          LSATISFY     
##  Min.   : 0.000   Very satisfied with life:114252  
##  1st Qu.: 0.000   Satisfied               :123445  
##  Median : 0.000   Dissatisfied            : 10758  
##  Mean   : 4.383   Very dissatisfied       :  3062  
##  3rd Qu.: 5.000   Dont know/Not sure      :  1864  
##  Max.   :30.000   Refused                 :  1107  
##  NA's   :9067     NA's                    :190644  
##                                     EMTSUPRT     
##  Always get social and emotional support:118012  
##  Usually                                : 77907  
##  Sometimes                              : 33813  
##  Rarely                                 : 10835  
##  Never                                  :  9379  
##  (Other)                                :  4195  
##  NA's                                   :190991  
##                                       SDHISOLT     
##  Never                                    :106160  
##  Rarely                                   : 70617  
##  Sometimes                                : 53072  
##  Usually                                  : 13178  
##  Always feel socially isolated from others:  8098  
##  (Other)                                  :  2665  
##  NA's                                     :191342  
##                                                                                                                             SDHSTRE1     
##  Never                                                                                                                          : 94681  
##  Rarely                                                                                                                         : 69465  
##  Sometimes                                                                                                                      : 55913  
##  Usually                                                                                                                        : 17179  
##  Always feels tense, restless, nervous, or anxious, or is unable to sleep at night because his/her mind is troubled all the time: 12295  
##  (Other)                                                                                                                        :  1678  
##  NA's                                                                                                                           :193921  
##                                   ADDEPEV3     
##  Yes ever told had depressive disorder: 91410  
##  No                                   :350910  
##  NA's                                 :  2812  
##                                                
##                                                
##                                                
##                                                
##                              PRIMINSR     
##  health ins thr employer or union:161388  
##  Medicare                        :135848  
##  private plan                    : 36931  
##  Medicaid                        : 29072  
##  no coverage of any type         : 23018  
##  (Other)                         : 58871  
##  NA's                            :     4  
##                                                                     CHECKUP1     
##  last routine checkup within past year (anytime less than 12 months ago):350944  
##  Within past 2 years (1 year but less than 2 years ago)                 : 41919  
##  Within past 5 years (2 years but less than 5 years ago)                : 24882  
##  5 or more years ago                                                    : 19079  
##  Dont know Not sure                                                     :  5063  
##  (Other)                                                                :  3242  
##  NA's                                                                   :     3  
##                            FLUSHOT7     
##  Yes got flu shot in last 12 mo:209256  
##  No                            :188755  
##  NA's                          : 47121  
##                                         
##                                         
##                                         
##                                         
##                                 COVIDPOS     
##  Yes had med prof tell positive test:110877  
##  No                                 :270055  
##  tested positive at home wo med prof: 13436  
##  NA's                               : 50764  
##                                              
##                                              
##                                              
##                                           COVIDSMP     
##  Yes had covid symptoms for more than 3 months: 26783  
##  No                                           : 94596  
##  NA's                                         :323753  
##                                                        
##                                                        
##                                                        
##                                                        
##                                                                                                        COVIDPRM     
##  Tiredness or fatigue                                                                                      :  7072  
##  Difficulty breathing or shortness of breath                                                               :  4772  
##  Loss of taste or smell                                                                                    :  4234  
##  Difficulty thinking or concentrating or forgetfulness/memory problems (sometimes referred to as brain fog):  2564  
##  Some other symptom                                                                                        :  2511  
##  (Other)                                                                                                   :  5562  
##  NA's                                                                                                      :418417  
##                          COVIDVA1                 COVIDNU1     
##  Yes had at least 1 covid vax:124818   had 1 covid vax:  6712  
##  No                          : 27011   2 covid vax    : 36926  
##  NA's                        :293303   3 covid vax    : 57488  
##                                        4 covid vax    : 23051  
##                                        NA's           :320955  
##                                                                
##                                                                
##                         EXERANY2         SLEPTIM1      Height_inches  
##  Yes exercised in past month:337559   Min.   : 1.000   Min.   :24.00  
##  No                         :106480   1st Qu.: 6.000   1st Qu.:64.00  
##  NA's                       :  1093   Median : 7.000   Median :67.00  
##                                       Mean   : 7.023   Mean   :67.06  
##                                       3rd Qu.: 8.000   3rd Qu.:70.00  
##                                       Max.   :24.000   Max.   :97.00  
##                                       NA's   :5453     NA's   :32468  
##     WEIGHT2           X_BMI5              X_BMI5CAT     
##  Min.   :  32.0   Min.   :12.02   Underweight  :  6778  
##  1st Qu.: 150.0   1st Qu.:24.13   Normal Weight:116976  
##  Median : 178.0   Median :27.44   Overweight   :139995  
##  Mean   : 183.3   Mean   :28.53   Obese        :132577  
##  3rd Qu.: 210.0   3rd Qu.:31.75   NA's         : 48806  
##  Max.   :1230.0   Max.   :99.64                         
##  NA's   :44257    NA's   :48806                         
##                                  SMOKE100                    SMOKDAY2     
##  yes smoked at least 100 cigs in life:164217   smoke every day   : 36003  
##  no                                  :245955   smoke some days   : 13938  
##  dont know not sure                  :  2297   not at all        :113774  
##  refused                             :   886   Dont know Not Sure:   165  
##  NA's                                : 31777   Refused           :   173  
##                                                NA's              :281079  
##                                                                           
##                             ECIGNOW2     
##  never used e-cigarettes in life:311988  
##  use every day                  : 10382  
##  use some days                  : 11734  
##  not at all right now           : 75368  
##  Dont know Not Sure             :   905  
##  Refused                        :  1176  
##  NA's                           : 33579  
##                            ALCDAY4          AVEDRNK3         DRNK3GE5     
##  none                          :187667   Min.   : 0.00    Min.   : 0.00   
##  1 in last month               : 31355   1st Qu.: 1.00    1st Qu.: 0.00   
##  2 in last month               : 24184   Median : 2.00    Median : 0.00   
##  1 alcoholic drink in past week: 19043   Mean   : 2.24    Mean   : 1.34   
##  2 in week                     : 15797   3rd Qu.: 3.00    3rd Qu.: 1.00   
##  (Other)                       :126323   Max.   :76.00    Max.   :76.00   
##  NA's                          : 40763   NA's   :237372   NA's   :238769  
##     MARIJAN1                       FIREARM5     
##  Min.   : 0       yes firearms in house: 13839  
##  1st Qu.: 0       no                   : 23428  
##  Median : 0       NA's                 :407865  
##  Mean   : 2                                     
##  3rd Qu.: 0                                     
##  Max.   :30                                     
##  NA's   :351397                                 
##                                                                                           ACEDEPRS     
##  Yes, Adverse Childhood Exper, lived with someone who was depressed, mentally ill, or suicidal:  8800  
##  No                                                                                           : 38156  
##  dont know not sure                                                                           :   489  
##  refused                                                                                      :   841  
##  NA's                                                                                         :396846  
##                                                                                                        
##                                                                                                        
##                                                                                     ACEDRINK     
##  Yes, Adverse Childhood Exper, lived with someone who was a problem drinker or alcoholic: 11527  
##  No                                                                                     : 35692  
##  dont know not sure                                                                     :   206  
##  refused                                                                                :   806  
##  NA's                                                                                   :396901  
##                                                                                                  
##                                                                                                  
##                                                                                                                   ACEDRUGS     
##  Yes, Adverse Childhood Exper, lived with someone who used illegal street drugs or who abused prescription medications:  4691  
##  No                                                                                                                   : 42448  
##  dont know not sure                                                                                                   :   307  
##  refused                                                                                                              :   774  
##  NA's                                                                                                                 :396912  
##                                                                                                                                
##                                                                                                                                
##                                                                                                                                               ACEPRISN     
##  Yes, Adverse Childhood Exper, lived with someone who served time or was sentenced to serve time in a prison, jail, or other correctional facility:  3549  
##  No                                                                                                                                               : 43701  
##  dont know not sure                                                                                                                               :   178  
##  refused                                                                                                                                          :   766  
##  NA's                                                                                                                                             :396938  
##                                                                                                                                                            
##                                                                                                                                                            
##                                                         ACEDIVRC     
##  Yes, Adverse Childhood Exper, parents separated or divorced: 12107  
##  No                                                         : 34367  
##  dont know not sure                                         :   269  
##  parents never married                                      :   632  
##  refused                                                    :   797  
##  NA's                                                       :396960  
##                                                                      
##                                                                                                                                 ACEPUNCH     
##  Adverse Childhood Exper, never: How often did your parents or adults in your home ever slap, hit, kick, punch or beat each other up: 38842  
##  once                                                                                                                               :  1795  
##  more than once                                                                                                                     :  5816  
##  dont know not sure                                                                                                                 :   672  
##  refused                                                                                                                            :  1004  
##  NA's                                                                                                                               :397003  
##                                                                                                                                              
##                                                                                                                                                                           ACEHURT1     
##  Adverse Childhood Exper, never: Not including spanking, (before age 18), how often did a parent or adult in your home ever hit, beat, kick, or physically hurt you in any way: 35092  
##  once                                                                                                                                                                         :  2939  
##  more than once                                                                                                                                                               :  8628  
##  dont know not sure                                                                                                                                                           :   350  
##  refused                                                                                                                                                                      :  1068  
##  NA's                                                                                                                                                                         :397055  
##                                                                                                                                                                                        
##                                                                                                                         ACESWEAR     
##  Adverse Childhood Exper, never: How often did a parent or adult in your home ever swear at you, insult you, or put you down: 30266  
##  once                                                                                                                       :  2393  
##  more than once                                                                                                             : 13722  
##  dont know not sure                                                                                                         :   573  
##  refused                                                                                                                    :  1075  
##  NA's                                                                                                                       :397103  
##                                                                                                                                      
##                                                                                                                        ACETOUCH     
##  Adverse Childhood Exper, never:  How often did anyone at least 5 years older than you or an adult, ever touch you sexually: 40941  
##  once                                                                                                                      :  1940  
##  more than once                                                                                                            :  3677  
##  dont know not sure                                                                                                        :   202  
##  refused                                                                                                                   :  1212  
##  NA's                                                                                                                      :397160  
##                                                                                                                                     
##                                              CIMEMLOS     
##  experienced confusion memory loss in last 12 mo :  7003  
##  no                                              : 56945  
##  NA's                                            :381184  
##                                                           
##                                                           
##                                                           
##                                                           
##                                                                                                                                                  CDHOUSE      
##  Never                                                                                                                                               :  3642  
##  Sometimes                                                                                                                                           :  1650  
##  Rarely                                                                                                                                              :  1272  
##  Usually                                                                                                                                             :   364  
##  Always, as a result of confusion or memory loss, how often have you given up day-to-day household activities or chores you used to do, in last 12 mo:   357  
##  (Other)                                                                                                                                             :   168  
##  NA's                                                                                                                                                :437679  
##                                                 CDASSIST     
##  Never                                              :  3954  
##  Rarely                                             :  1447  
##  Sometimes                                          :  1354  
##  Always need assist because confusion or memory loss:   304  
##  Usually                                            :   281  
##  (Other)                                            :    91  
##  NA's                                               :437701  
##                                                                      CDSOCIAL     
##  Never                                                                   :  3726  
##  Rarely                                                                  :  1387  
##  Sometimes                                                               :  1313  
##  Always confusion or memory loss interfere with work or social activities:   458  
##  Usually                                                                 :   410  
##  (Other)                                                                 :   121  
##  NA's                                                                    :437717  
##                                                                   CAREGIV1     
##  provided care to family or friend with disability or health condition: 19662  
##  no                                                                   : 78174  
##  caregiving recipient died in past 30 days                            :   177  
##  NA's                                                                 :347119  
##                                                                                
##                                                                                
##                                                                                
##                        CRGVREL4        Age_midpt    
##  Mother                    :  3986   Min.   :21.00  
##  Non-relative/Family friend:  3002   1st Qu.:42.00  
##  Husband                   :  2243   Median :57.00  
##  Child                     :  1968   Mean   :55.06  
##  Wife                      :  1904   3rd Qu.:72.00  
##  (Other)                   :  6531   Max.   :82.00  
##  NA's                      :425498   NA's   :9079
summary(brfss_ACE)
##          X_STATE                        X_METSTAT              X_URBSTAT    
##  Florida     :9593   Metropolitan counties   :30170   urban counties:38265  
##  Virginia    :8024   nonmetropolitan counties:17802   rural counties: 9707  
##  Iowa        :7817                                                          
##  South Dakota:7112                                                          
##  Oregon      :5066                                                          
##  Arkansas    :4180                                                          
##  (Other)     :6180                                                          
##                                MSCODE         CHILDREN      
##  in central city                  : 3195   Min.   : 0.0000  
##  in county containing central city: 1439   1st Qu.: 0.0000  
##  in suburb near city              : 2554   Median : 0.0000  
##  outside MSA                      : 4986   Mean   : 0.4767  
##  NA's                             :35798   3rd Qu.: 0.0000  
##                                            Max.   :82.0000  
##                                            NA's   :447      
##                             MARITAL     
##  Married                        :24842  
##  Divorced                       : 6346  
##  Widowed                        : 6106  
##  Separated                      :  844  
##  Never married                  : 7645  
##  A member of an unmarried couple: 1805  
##  Refused                        :  384  
##                                                           EDUCA      
##  Never attended school or only kindergarten                  :   62  
##  Grades 1 through 8 (Elementary)                             :  853  
##  Grades 9 through 11 (Some high school)                      : 1821  
##  Grade 12 or GED (High school graduate)                      :12363  
##  College 1 year to 3 years (Some college or technical school):13931  
##  College 4 years or more (College graduate)                  :18741  
##  Refused                                                     :  201  
##           VETERAN3                                  X_PRACE2    
##  Yes a veteran: 6661   White                            :40007  
##  No           :41182   Black or African American        : 3167  
##  NA's         :  129   American Indian or Alaskan Native: 1717  
##                        Refused                          : 1092  
##                        Asian                            :  682  
##                        No race choice given             :  569  
##                        (Other)                          :  738  
##                      X_HISPANC               X_AGEG5YR         RENTHOM1    
##  yes Hispanic             : 2920   Age 65 to 69   : 5371   own home:35496  
##  no                       :44558   Age 70 to 74   : 5100   rent    : 9980  
##  dont know refused missing:  494   Age 60 to 64   : 4876   other   : 2075  
##                                    Age 80 or older: 4642   NA's    :  421  
##                                    Age 75 to 79   : 4017                   
##                                    Age 55 to 59   : 3966                   
##                                    (Other)        :20000                   
##                EMPLOY1     
##  Employed for wages:19250  
##  Retired           :16734  
##  Self-employed     : 4338  
##  Unable to work    : 2958  
##  A homemaker       : 1709  
##  (Other)           : 2520  
##  NA's              :  463  
##                                                 INCOME3     
##  Less than $75,000 ($50,000 to less than $75,000)   : 7065  
##  Less than $50,000 ($35,000 to less than $50,000)   : 5771  
##  Less than $100,000 ($75,000 to less than $100,000) : 5607  
##  Less than $150,000 ($100,000 to less than $150,000): 5230  
##  Refused                                            : 5005  
##  (Other)                                            :19293  
##  NA's                                               :    1  
##                  FOODSTMP    
##  got food stamps SNAP: 1999  
##  no                  :17439  
##  NA's                :28534  
##                              
##                              
##                              
##                              
##                                                                                               SDHFOOD1    
##  Never                                                                                            :15582  
##  Rarely                                                                                           : 1705  
##  Sometimes                                                                                        : 1363  
##  Always the food that you bought not last, and you didn\x92t have money to get more, in last 12 mo:  392  
##  Usually                                                                                          :  361  
##  (Other)                                                                                          :   78  
##  NA's                                                                                             :28491  
##     SEXVAR                     BIRTHSEX                            SOMALE     
##  Male  :22264   male sex at birth  : 4955   Gay                       :  206  
##  Female:25708   female sex at birth: 5283   Straight, that is, not gay: 9322  
##                 NA's               :37734   Bisexual                  :  198  
##                                             Something else            :  130  
##                                             I dont know the answer    :   65  
##                                             Refused                   :  208  
##                                             NA's                      :37843  
##                        SOFEMALE    
##  Lesbian or Gay            :  151  
##  Straight, that is, not gay:10540  
##  Bisexual                  :  428  
##  Something else            :  192  
##  I dont know the answer    :  116  
##  Refused                   :  265  
##  NA's                      :36280  
##                                    TRNSGNDR    
##  Yes, Transgender, male-to-female      :   34  
##  Yes, Transgender, female to male      :   32  
##  Yes, Transgender, gender nonconforming:   33  
##  No                                    :21360  
##  Dont know/Not Sure                    :   54  
##  Refused                               :  294  
##  NA's                                  :26165  
##                       HADSEX                      GENHLTH         PHYSHLTH     
##  yes had sex in last 6 mo: 3166   Excellent           : 7038   Min.   : 0.000  
##  no                      :  836   Very good           :15770   1st Qu.: 0.000  
##  dont know not sure      :   17   Good                :15920   Median : 0.000  
##  refused                 :  178   Fair                : 6851   Mean   : 4.497  
##  NA's                    :43775   Poor                : 2265   3rd Qu.: 4.000  
##                                   Dont know - Not Sure:   92   Max.   :30.000  
##                                   Refused             :   36   NA's   :1078    
##     MENTHLTH                          LSATISFY    
##  Min.   : 0.000   Very satisfied with life: 9184  
##  1st Qu.: 0.000   Satisfied               : 9120  
##  Median : 0.000   Dissatisfied            :  803  
##  Mean   : 4.255   Very dissatisfied       :  276  
##  3rd Qu.: 4.000   Dont know/Not sure      :  105  
##  Max.   :30.000   Refused                 :   83  
##  NA's   :890      NA's                    :28401  
##                                     EMTSUPRT    
##  Always get social and emotional support: 9296  
##  Usually                                : 6018  
##  Sometimes                              : 2626  
##  Rarely                                 :  844  
##  Never                                  :  533  
##  (Other)                                :  233  
##  NA's                                   :28422  
##                                       SDHISOLT    
##  Never                                    : 8397  
##  Rarely                                   : 5218  
##  Sometimes                                : 4095  
##  Usually                                  : 1027  
##  Always feel socially isolated from others:  629  
##  (Other)                                  :  165  
##  NA's                                     :28441  
##                                                                                                                             SDHSTRE1    
##  Never                                                                                                                          : 7541  
##  Rarely                                                                                                                         : 5425  
##  Sometimes                                                                                                                      : 4178  
##  Usually                                                                                                                        : 1248  
##  Always feels tense, restless, nervous, or anxious, or is unable to sleep at night because his/her mind is troubled all the time:  912  
##  (Other)                                                                                                                        :   79  
##  NA's                                                                                                                           :28589  
##                                   ADDEPEV3    
##  Yes ever told had depressive disorder: 9558  
##  No                                   :38108  
##  NA's                                 :  306  
##                                               
##                                               
##                                               
##                                               
##                              PRIMINSR    
##  Medicare                        :16333  
##  health ins thr employer or union:16166  
##  private plan                    : 4069  
##  Medicaid                        : 2816  
##  no coverage of any type         : 2214  
##  CHAMPUS                         : 2039  
##  (Other)                         : 4335  
##                                                                     CHECKUP1    
##  last routine checkup within past year (anytime less than 12 months ago):38657  
##  Within past 2 years (1 year but less than 2 years ago)                 : 4030  
##  Within past 5 years (2 years but less than 5 years ago)                : 2396  
##  5 or more years ago                                                    : 2038  
##  Dont know Not sure                                                     :  542  
##  Never                                                                  :  248  
##  Refused                                                                :   61  
##                            FLUSHOT7    
##  Yes got flu shot in last 12 mo:25103  
##  No                            :22389  
##  NA's                          :  480  
##                                        
##                                        
##                                        
##                                        
##                                 COVIDPOS    
##  Yes had med prof tell positive test:13112  
##  No                                 :33123  
##  tested positive at home wo med prof: 1497  
##  NA's                               :  240  
##                                             
##                                             
##                                             
##                                           COVIDSMP    
##  Yes had covid symptoms for more than 3 months: 3429  
##  No                                           :10831  
##  NA's                                         :33712  
##                                                       
##                                                       
##                                                       
##                                                       
##                                                                                                        COVIDPRM    
##  Tiredness or fatigue                                                                                      :  895  
##  Difficulty breathing or shortness of breath                                                               :  655  
##  Loss of taste or smell                                                                                    :  565  
##  Some other symptom                                                                                        :  321  
##  Difficulty thinking or concentrating or forgetfulness/memory problems (sometimes referred to as brain fog):  313  
##  (Other)                                                                                                   :  680  
##  NA's                                                                                                      :44543  
##                          COVIDVA1                COVIDNU1    
##  Yes had at least 1 covid vax:12319   had 1 covid vax:  785  
##  No                          : 3253   2 covid vax    : 3931  
##  NA's                        :32400   3 covid vax    : 5648  
##                                       4 covid vax    : 1913  
##                                       NA's           :35695  
##                                                              
##                                                              
##                         EXERANY2        SLEPTIM1      Height_inches  
##  Yes exercised in past month:35631   Min.   : 1.000   Min.   :36.00  
##  No                         :12229   1st Qu.: 6.000   1st Qu.:64.00  
##  NA's                       :  112   Median : 7.000   Median :67.00  
##                                      Mean   : 7.077   Mean   :67.21  
##                                      3rd Qu.: 8.000   3rd Qu.:70.00  
##                                      Max.   :24.000   Max.   :96.00  
##                                      NA's   :511      NA's   :1307   
##     WEIGHT2           X_BMI5              X_BMI5CAT    
##  Min.   :  60.0   Min.   :12.11   Underweight  :  744  
##  1st Qu.: 150.0   1st Qu.:24.37   Normal Weight:12381  
##  Median : 180.0   Median :27.76   Overweight   :15754  
##  Mean   : 186.3   Mean   :28.85   Obese        :15997  
##  3rd Qu.: 210.0   3rd Qu.:32.11   NA's         : 3096  
##  Max.   :1135.0   Max.   :95.66                        
##  NA's   :2689     NA's   :3096                         
##                                  SMOKE100                   SMOKDAY2    
##  yes smoked at least 100 cigs in life:20035   smoke every day   : 4626  
##  no                                  :27551   smoke some days   : 1669  
##  dont know not sure                  :  264   not at all        :13705  
##  refused                             :  122   Dont know Not Sure:   20  
##                                               Refused           :   15  
##                                               NA's              :27937  
##                                                                         
##                             ECIGNOW2                               ALCDAY4     
##  never used e-cigarettes in life:37306   none                          :22579  
##  use every day                  : 1168   1 in last month               : 3692  
##  use some days                  : 1253   2 in last month               : 2956  
##  not at all right now           : 7969   1 alcoholic drink in past week: 2133  
##  Dont know Not Sure             :  127   30 in last month              : 1967  
##  Refused                        :  149   2 in week                     : 1693  
##                                          (Other)                       :12952  
##     AVEDRNK3         DRNK3GE5         MARIJAN1     
##  Min.   : 0.000   Min.   : 0.000   Min.   : 0.000  
##  1st Qu.: 1.000   1st Qu.: 0.000   1st Qu.: 0.000  
##  Median : 2.000   Median : 0.000   Median : 0.000  
##  Mean   : 2.276   Mean   : 1.408   Mean   : 2.219  
##  3rd Qu.: 3.000   3rd Qu.: 1.000   3rd Qu.: 0.000  
##  Max.   :76.000   Max.   :55.000   Max.   :30.000  
##  NA's   :23573    NA's   :23662    NA's   :29361   
##                   FIREARM5    
##  yes firearms in house:  920  
##  no                   : 1276  
##  NA's                 :45776  
##                               
##                               
##                               
##                               
##                                                                                           ACEDEPRS    
##  Yes, Adverse Childhood Exper, lived with someone who was depressed, mentally ill, or suicidal: 8762  
##  No                                                                                           :37925  
##  dont know not sure                                                                           :  481  
##  refused                                                                                      :  804  
##                                                                                                       
##                                                                                                       
##                                                                                                       
##                                                                                     ACEDRINK    
##  Yes, Adverse Childhood Exper, lived with someone who was a problem drinker or alcoholic:11472  
##  No                                                                                     :35516  
##  dont know not sure                                                                     :  201  
##  refused                                                                                :  783  
##                                                                                                 
##                                                                                                 
##                                                                                                 
##                                                                                                                   ACEDRUGS    
##  Yes, Adverse Childhood Exper, lived with someone who used illegal street drugs or who abused prescription medications: 4664  
##  No                                                                                                                   :42247  
##  dont know not sure                                                                                                   :  305  
##  refused                                                                                                              :  756  
##                                                                                                                               
##                                                                                                                               
##                                                                                                                               
##                                                                                                                                               ACEPRISN    
##  Yes, Adverse Childhood Exper, lived with someone who served time or was sentenced to serve time in a prison, jail, or other correctional facility: 3528  
##  No                                                                                                                                               :43523  
##  dont know not sure                                                                                                                               :  174  
##  refused                                                                                                                                          :  747  
##                                                                                                                                                           
##                                                                                                                                                           
##                                                                                                                                                           
##                                                         ACEDIVRC    
##  Yes, Adverse Childhood Exper, parents separated or divorced:12050  
##  No                                                         :34244  
##  dont know not sure                                         :  269  
##  parents never married                                      :  626  
##  refused                                                    :  783  
##                                                                     
##                                                                     
##                                                                                                                                 ACEPUNCH    
##  Adverse Childhood Exper, never: How often did your parents or adults in your home ever slap, hit, kick, punch or beat each other up:38729  
##  once                                                                                                                               : 1791  
##  more than once                                                                                                                     : 5799  
##  dont know not sure                                                                                                                 :  663  
##  refused                                                                                                                            :  990  
##                                                                                                                                             
##                                                                                                                                             
##                                                                                                                                                                           ACEHURT1    
##  Adverse Childhood Exper, never: Not including spanking, (before age 18), how often did a parent or adult in your home ever hit, beat, kick, or physically hurt you in any way:35028  
##  once                                                                                                                                                                         : 2933  
##  more than once                                                                                                                                                               : 8607  
##  dont know not sure                                                                                                                                                           :  349  
##  refused                                                                                                                                                                      : 1055  
##                                                                                                                                                                                       
##                                                                                                                                                                                       
##                                                                                                                         ACESWEAR    
##  Adverse Childhood Exper, never: How often did a parent or adult in your home ever swear at you, insult you, or put you down:30237  
##  once                                                                                                                       : 2392  
##  more than once                                                                                                             :13710  
##  dont know not sure                                                                                                         :  570  
##  refused                                                                                                                    : 1063  
##                                                                                                                                     
##                                                                                                                                     
##                                                                                                                        ACETOUCH    
##  Adverse Childhood Exper, never:  How often did anyone at least 5 years older than you or an adult, ever touch you sexually:40941  
##  once                                                                                                                      : 1940  
##  more than once                                                                                                            : 3677  
##  dont know not sure                                                                                                        :  202  
##  refused                                                                                                                   : 1212  
##                                                                                                                                    
##                                                                                                                                    
##                                              CIMEMLOS    
##  experienced confusion memory loss in last 12 mo : 2151  
##  no                                              :16486  
##  NA's                                            :29335  
##                                                          
##                                                          
##                                                          
##                                                          
##                                                                                                                                                  CDHOUSE     
##  Never                                                                                                                                               : 1108  
##  Sometimes                                                                                                                                           :  490  
##  Rarely                                                                                                                                              :  409  
##  Usually                                                                                                                                             :  122  
##  Always, as a result of confusion or memory loss, how often have you given up day-to-day household activities or chores you used to do, in last 12 mo:  105  
##  (Other)                                                                                                                                             :   45  
##  NA's                                                                                                                                                :45693  
##                                                 CDASSIST    
##  Never                                              : 1222  
##  Rarely                                             :  444  
##  Sometimes                                          :  390  
##  Always need assist because confusion or memory loss:  105  
##  Usually                                            :   94  
##  (Other)                                            :   24  
##  NA's                                               :45693  
##                                                                      CDSOCIAL    
##  Never                                                                   : 1150  
##  Rarely                                                                  :  434  
##  Sometimes                                                               :  375  
##  Usually                                                                 :  146  
##  Always confusion or memory loss interfere with work or social activities:  138  
##  (Other)                                                                 :   36  
##  NA's                                                                    :45693  
##                                                                   CAREGIV1    
##  provided care to family or friend with disability or health condition: 2711  
##  no                                                                   :10165  
##  caregiving recipient died in past 30 days                            :   31  
##  NA's                                                                 :35065  
##                                                                               
##                                                                               
##                                                                               
##                        CRGVREL4       Age_midpt    ACEDRINK_recode
##  Mother                    :  584   Min.   :21.0    :47972        
##  Non-relative/Family friend:  456   1st Qu.:42.0                  
##  Husband                   :  301   Median :62.0                  
##  Child                     :  270   Mean   :56.8                  
##  Wife                      :  245   3rd Qu.:72.0                  
##  (Other)                   :  854   Max.   :82.0                  
##  NA's                      :45262   NA's   :803
