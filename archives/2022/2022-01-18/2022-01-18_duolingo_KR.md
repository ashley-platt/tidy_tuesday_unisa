Duolingo Data for UniSA TidyTuesday Github
================

    ## Warning: package 'tidyverse' was built under R version 4.1.1

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.2     v dplyr   1.0.7
    ## v tidyr   1.1.4     v stringr 1.4.0
    ## v readr   2.1.1     v forcats 0.5.1

    ## Warning: package 'tidyr' was built under R version 4.1.2

    ## Warning: package 'readr' was built under R version 4.1.2

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

    ## 
    ## Attaching package: 'magrittr'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     set_names

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     extract

    ## Warning: package 'janitor' was built under R version 4.1.2

    ## 
    ## Attaching package: 'janitor'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

    ## Warning: package 'ggpubr' was built under R version 4.1.2

    ## Warning: package 'mapsf' was built under R version 4.1.2

    ## Loading required package: sf

    ## Warning: package 'sf' was built under R version 4.1.2

    ## Linking to GEOS 3.9.1, GDAL 3.2.1, PROJ 7.2.1; sf_use_s2() is TRUE

    ## Warning: package 'rnaturalearth' was built under R version 4.1.2

    ## Warning: package 'rnaturalearthdata' was built under R version 4.1.2

``` r
duo_data<-read.csv("2022-01-18_duolingo-data_KR.csv")
head(duo_data)
```

    ##           country    pop1    pop2    pop3    pop1PC    pop2PC     pop3PC
    ## 1     Afghanistan English Spanish  German 0.1892418 0.1454583 0.12224616
    ## 2         Albania  German English Italian 0.3267552 0.1314111 0.11014675
    ## 3         Algeria English  French Spanish 0.5230045 0.1963351 0.08324474
    ## 4         Andorra English  French Spanish 0.4020675 0.1718050 0.11134568
    ## 5          Angola English  French Spanish 0.6320583 0.1217486 0.09208257
    ## 6 Antigua-Barbuda Spanish  French English 0.4429754 0.2201775 0.10297183
    ##        pcEN       pcES      pcFR motivSchool  motivWork motivTravel motivBrain
    ## 1 0.1892418 0.14545834 0.1039683   0.1972353 0.14703838  0.17391304  0.1279635
    ## 2 0.1314111 0.09388738 0.1051256   0.2554075 0.17209740  0.09842776  0.1365708
    ## 3 0.5230045 0.08324474 0.1963351   0.2339340 0.09985178  0.12768909  0.1392341
    ## 4 0.4020675 0.11134568 0.1718050   0.2124346 0.20955497  0.19607330  0.1071990
    ## 5 0.6320583 0.09208257 0.1217486   0.3277368 0.19435827  0.10209349  0.1412780
    ## 6 0.1029718 0.44297540 0.2201775   0.2526166 0.13659748  0.13251730  0.1848501
    ##   motivFamily motivCulture motivOther        ui1     ui2     ui1PC      ui2PC
    ## 1   0.1364584   0.09128118 0.12611012    English  Arabic 0.5970186 0.05278338
    ## 2   0.1172435   0.10937064 0.11088241    English Italian 0.6118223 0.06138479
    ## 3   0.1699585   0.13652607 0.09280651     French  Arabic 0.3880508 0.36252946
    ## 4   0.0736911   0.11073298 0.09031414    Spanish English 0.5413315 0.17205984
    ## 5   0.0957322   0.04989963 0.08890164 Portuguese English 0.6921190 0.10026715
    ## 6   0.1188575   0.05925138 0.11530956    English Spanish 0.7132988 0.08337800
    ##   covidPop1 covidPop2 covidPop3 covidSchool covidWork covidTravel covidBrain
    ## 1   English   Turkish   Spanish   0.1708312 0.1626721  0.15910250  0.1392147
    ## 2    German   English   Italian   0.2246981 0.1643261  0.09685443  0.1549464
    ## 3   English    French   Spanish   0.2109723 0.1008146  0.13909673  0.1391368
    ## 4   English    French   Catalan   0.2079353 0.2358560  0.14621602  0.1234386
    ## 5   English    French   Spanish   0.3340847 0.1944936  0.08839156  0.1447432
    ## 6   Spanish    French   English   0.2510417 0.1375000  0.11145833  0.2010417
    ##   covidFamily covidCulture covidOther
    ## 1  0.13411525   0.09280979 0.14125446
    ## 2  0.11333368   0.13613515 0.10970617
    ## 3  0.16232818   0.15020867 0.09744271
    ## 4  0.07274063   0.11903013 0.09478325
    ## 5  0.09064563   0.05780068 0.08984061
    ## 6  0.12291667   0.06875000 0.10729167

``` r
#collect all unique non-NA levels from pop1, pop2, pop3 as a list
langs<-unique(c(levels(as.factor(duo_data$pop1)),levels((as.factor(duo_data$pop2))),levels((as.factor(duo_data$pop3)))))
langs
```

    ##  [1] "English"    "French"     "German"     "Irish"      "Japanese"  
    ##  [6] "Korean"     "Spanish"    "Swedish"    "Chinese"    "Danish"    
    ## [11] "Hindi"      "Italian"    "Norwegian"  "Portuguese" "Russian"   
    ## [16] "Swahili"    "Turkish"    "Arabic"     "Dutch"      "Finnish"   
    ## [21] "Greek"      "Guarani"    "Hebrew"     "Vietnamese"

``` r
#add each language in the list to the dataframe, and collect percentage of users from relevant column, depending on rank in which it appeared
for (l in langs) {
  duo_data[,l]= case_when (duo_data$pop1==l ~ duo_data$pop1PC, duo_data$pop2==l ~ duo_data$pop2PC, duo_data$pop3==l ~ duo_data$pop3PC)         
}

#check that this has worked
colnames(duo_data)
```

    ##  [1] "country"      "pop1"         "pop2"         "pop3"         "pop1PC"      
    ##  [6] "pop2PC"       "pop3PC"       "pcEN"         "pcES"         "pcFR"        
    ## [11] "motivSchool"  "motivWork"    "motivTravel"  "motivBrain"   "motivFamily" 
    ## [16] "motivCulture" "motivOther"   "ui1"          "ui2"          "ui1PC"       
    ## [21] "ui2PC"        "covidPop1"    "covidPop2"    "covidPop3"    "covidSchool" 
    ## [26] "covidWork"    "covidTravel"  "covidBrain"   "covidFamily"  "covidCulture"
    ## [31] "covidOther"   "English"      "French"       "German"       "Irish"       
    ## [36] "Japanese"     "Korean"       "Spanish"      "Swedish"      "Chinese"     
    ## [41] "Danish"       "Hindi"        "Italian"      "Norwegian"    "Portuguese"  
    ## [46] "Russian"      "Swahili"      "Turkish"      "Arabic"       "Dutch"       
    ## [51] "Finnish"      "Greek"        "Guarani"      "Hebrew"       "Vietnamese"

``` r
#count non-NAs
times_ranked<-colSums(!is.na(select(duo_data,English:Vietnamese)))

#put in descending order
times_ranked<-times_ranked %>%
  sort(decreasing=TRUE) %>%
  names()

#add "country" in front
times_ranked<-append(times_ranked,"country", after=0)

#select data from main dataframe (in new order) and drop empty rows
toplang<-duo_data %>% 
  select(all_of(times_ranked)) %>%
  select(1:6) %>%
  remove_empty(which= "rows")
toplang
```

    ##                   country    English    Spanish     French     German
    ## 1             Afghanistan 0.18924178 0.14545834         NA 0.12224616
    ## 2                 Albania 0.13141109         NA         NA 0.32675524
    ## 3                 Algeria 0.52300448 0.08324474 0.19633508         NA
    ## 4                 Andorra 0.40206748 0.11134568 0.17180504         NA
    ## 5                  Angola 0.63205829 0.09208257 0.12174863         NA
    ## 6         Antigua-Barbuda 0.10297183 0.44297540 0.22017748         NA
    ## 7               Argentina 0.52344813         NA 0.09279800         NA
    ## 8                 Armenia 0.60901287 0.07234515 0.08244495         NA
    ## 9               Australia         NA 0.17173054 0.16129154         NA
    ## 10                Austria 0.27109380 0.17105027         NA 0.13678023
    ## 11             Azerbaijan 0.48953451         NA         NA 0.08121756
    ## 12                Bahamas 0.07535895 0.43444823 0.18048524         NA
    ## 13                Bahrain 0.40029964 0.11367573 0.11993071         NA
    ## 14             Bangladesh 0.11240693 0.16963729 0.14099723         NA
    ## 15               Barbados         NA 0.42599653 0.21219430         NA
    ## 16                Belarus 0.63787965         NA 0.06118136 0.10364581
    ## 17                Belgium 0.30683570 0.16671173 0.13388012         NA
    ## 18                 Belize 0.20264335 0.34959605 0.11577379         NA
    ## 19                  Benin 0.65715163 0.09949136 0.09177409         NA
    ## 20                 Bhutan         NA 0.14911661         NA         NA
    ## 21                Bolivia 0.64703695         NA 0.07323956         NA
    ## 22     Bosnia-Herzegovina 0.08573126 0.13462080         NA 0.34651428
    ## 23               Botswana         NA 0.20740294 0.32247033         NA
    ## 24                 Brazil 0.60882245 0.15318205 0.07792976         NA
    ## 25                 Brunei         NA         NA         NA         NA
    ## 26               Bulgaria 0.21240905 0.15822529         NA 0.15239780
    ## 27           Burkina-Faso 0.72553263 0.07994589 0.07379100         NA
    ## 28             Cabo-Verde 0.56251915 0.10331750 0.13201042         NA
    ## 29               Cambodia 0.20659361 0.13249102         NA         NA
    ## 30               Cameroon 0.55668385 0.11129475         NA 0.10759706
    ## 31                 Canada 0.13891205 0.19957165 0.30143675         NA
    ## 32                   Chad 0.60671463 0.07279890 0.21068859         NA
    ## 33                  Chile 0.60493241         NA 0.08913802         NA
    ## 34                  China 0.44416044         NA         NA         NA
    ## 35               Colombia 0.69311113         NA 0.09251597         NA
    ## 36                  Congo 0.63674401 0.11220997 0.08092431         NA
    ## 37             Costa-Rica 0.58782816         NA 0.08748430         NA
    ## 38           Cote-dIvoire 0.69149824 0.09797615 0.06703986         NA
    ## 39                Croatia         NA 0.17322325         NA 0.21799342
    ## 40                   Cuba 0.58831275 0.08407275 0.09343710         NA
    ## 41                 Cyprus 0.26026134 0.15895465         NA         NA
    ## 42                Czechia 0.53800718 0.09085111         NA 0.08657095
    ## 43   Democratic-Rep-Congo 0.59849233 0.11457118 0.11298573         NA
    ## 44                Denmark         NA 0.17988330         NA 0.14543152
    ## 45               Djibouti 0.44693238 0.13569189 0.18327362         NA
    ## 46               Dominica 0.07902531 0.35339257 0.31744750         NA
    ## 47          Dominican-Rep 0.72880638         NA 0.07072919         NA
    ## 48                Ecuador 0.65454859         NA 0.09028107         NA
    ## 49                  Egypt 0.50231589         NA 0.11184844 0.11342521
    ## 50            El-Salvador 0.68790930         NA 0.08641953         NA
    ## 51                Estonia 0.17203290 0.15683847         NA         NA
    ## 52               Ethiopia 0.09314791 0.20837676 0.26651976         NA
    ## 53                   Fiji 0.09712120 0.21760066 0.18901752         NA
    ## 54                Finland         NA 0.16254796         NA         NA
    ## 55                 France 0.45609391 0.15922182 0.09109026         NA
    ## 56                  Gabon 0.63812478 0.13718412 0.06660904         NA
    ## 57                 Gambia 0.13706461 0.16790747 0.29566605         NA
    ## 58                Georgia 0.27283073 0.11521109         NA 0.13882734
    ## 59                Germany 0.30884701 0.16997690         NA 0.15035334
    ## 60                  Ghana         NA 0.22990290 0.37401021 0.09406725
    ## 61          Great-Britain         NA 0.24154774 0.19196037 0.08220350
    ## 62                 Greece 0.35074585 0.15468034 0.08254582         NA
    ## 63                Grenada         NA 0.41591459 0.21329518         NA
    ## 64              Guatemala 0.71195352         NA 0.07562654         NA
    ## 65                 Guinea 0.69026965 0.08277592 0.09275711         NA
    ## 66                 Guyana         NA 0.49063102 0.11183653         NA
    ## 67                  Haiti 0.61325294 0.19656788 0.07294984         NA
    ## 68               Honduras 0.67749244         NA 0.08733260         NA
    ## 69                Hungary 0.56334567 0.07042720         NA 0.09053169
    ## 70                Iceland         NA 0.23463303 0.09609680         NA
    ## 71                  India 0.40504460         NA 0.10825975         NA
    ## 72              Indonesia 0.52039465         NA         NA         NA
    ## 73                   Iran 0.22285488         NA 0.14997440 0.16028697
    ## 74                   Iraq 0.69531955         NA 0.06880424         NA
    ## 75                Ireland         NA 0.18812915 0.15905928         NA
    ## 76                 Israel 0.23490307 0.16624055         NA         NA
    ## 77                  Italy 0.52189122         NA 0.10439820         NA
    ## 78                Jamaica         NA 0.47649020 0.18436384         NA
    ## 79                  Japan 0.64976708         NA         NA         NA
    ## 80                 Jordan 0.58888604         NA 0.08149349         NA
    ## 81             Kazakhstan 0.67698316         NA 0.06716830 0.07053977
    ## 82                  Kenya         NA 0.23445243 0.25910954 0.08631502
    ## 83                 Kuwait 0.42508021 0.11199295 0.12513484         NA
    ## 84             Kyrgyzstan 0.66925844         NA 0.05443297 0.09782164
    ## 85                    Lao 0.51006651 0.10403559 0.07545389         NA
    ## 86                 Latvia 0.26498352 0.14189056         NA 0.13035554
    ## 87                Lebanon 0.31735819 0.15530027 0.13917784         NA
    ## 88                  Libya 0.66982388         NA 0.09243722 0.05055033
    ## 89              Lithuania 0.21442515 0.15324928         NA         NA
    ## 90             Luxembourg 0.22522387 0.13336254 0.21659090         NA
    ## 91              Macedonia 0.09098955 0.13796427         NA 0.30888065
    ## 92             Madagascar 0.53496681 0.12128971         NA 0.09068445
    ## 93                 Malawi         NA 0.17741438 0.28093181         NA
    ## 94               Malaysia         NA         NA         NA         NA
    ## 95               Maldives 0.13253476 0.20622745 0.11267556         NA
    ## 96                   Mali 0.69629470 0.07906801 0.11331643         NA
    ## 97                  Malta 0.17572380 0.18065430         NA         NA
    ## 98             Mauritania 0.50916205 0.09060972 0.25973264         NA
    ## 99              Mauritius 0.19847400 0.22134064 0.18443103         NA
    ## 100                Mexico 0.67417577         NA 0.10040808         NA
    ## 101               Moldova 0.62263965         NA 0.07081847 0.08186411
    ## 102                Monaco 0.26206123         NA 0.24811669         NA
    ## 103              Mongolia 0.21459753         NA         NA         NA
    ## 104            Montenegro         NA 0.16829048         NA 0.17568461
    ## 105               Morocco 0.49375693 0.10737432 0.22261488         NA
    ## 106            Mozambique 0.59069113         NA 0.09506841         NA
    ## 107               Myanmar         NA         NA         NA         NA
    ## 108               Namibia         NA 0.19022140 0.15840569 0.20874051
    ## 109                 Nepal 0.25511066 0.15843872 0.09538815         NA
    ## 110           Netherlands 0.28982949 0.17113865         NA         NA
    ## 111           New-Zealand         NA 0.22305390 0.16678264         NA
    ## 112             Nicaragua 0.69102969         NA 0.07716054         NA
    ## 113                 Niger 0.56070826 0.10694210 0.17228780         NA
    ## 114               Nigeria         NA 0.24281055 0.35328882 0.08788268
    ## 115                Norway         NA 0.23167737 0.11940903         NA
    ## 116                  Oman 0.58375752 0.07239176 0.08431363         NA
    ## 117              Pakistan 0.19790067 0.11459796         NA         NA
    ## 118                Panama 0.66526769         NA 0.08321840         NA
    ## 119              Paraguay 0.50139828         NA         NA         NA
    ## 120                  Peru 0.62802240         NA 0.08846739         NA
    ## 121           Philippines         NA 0.20529297         NA         NA
    ## 122                Poland 0.58280308 0.08646498         NA 0.06869044
    ## 123              Portugal 0.39613618 0.11404710 0.14052742         NA
    ## 124                 Qatar 0.30509127 0.13691942 0.14004258         NA
    ## 125               Romania 0.47524405 0.09191756         NA 0.08974207
    ## 126                Russia 0.63161165         NA 0.07273270 0.09277423
    ## 127                Rwanda 0.16847359 0.19286698 0.28588023         NA
    ## 128          Saudi-Arabia 0.71736958 0.04299506 0.06705128         NA
    ## 129               Senegal 0.60614229 0.11683131 0.10132756         NA
    ## 130                Serbia 0.09332369 0.17054376         NA 0.23624057
    ## 131            Seychelles         NA 0.16569390 0.17347600 0.15700389
    ## 132          Sierra-Leone 0.09793405 0.17918156 0.39292809         NA
    ## 133             Singapore 0.12884131 0.12362391         NA         NA
    ## 134              Slovakia 0.37209092 0.11693640         NA 0.12820966
    ## 135              Slovenia         NA 0.17504990         NA 0.23145394
    ## 136               Somalia 0.28758258         NA         NA         NA
    ## 137          South-Africa         NA 0.19658843 0.20424526 0.10804025
    ## 138           South-Korea 0.56999285 0.07804245         NA         NA
    ## 139                 Spain 0.46136883 0.09868025 0.13068044         NA
    ## 140             Sri-Lanka 0.09807659 0.15638744 0.18608256         NA
    ## 141              St-Lucia 0.04174741 0.41226953 0.30164443         NA
    ## 142 St-Vincent-Grenadines 0.04595669 0.44940345 0.25994255         NA
    ## 143                 Sudan 0.68312865         NA 0.10645233 0.04724743
    ## 144              Suriname 0.27061908 0.25435236 0.12095853         NA
    ## 145                Sweden         NA 0.17348186 0.10914544         NA
    ## 146           Switzerland 0.27960572         NA 0.17433163 0.16433298
    ## 147            Tajikistan 0.64275548         NA         NA 0.12278205
    ## 148              Tanzania         NA 0.17997381 0.19505727         NA
    ## 149              Thailand 0.53853894 0.07902411         NA         NA
    ## 150                  Togo 0.56040294 0.12504734 0.10618799         NA
    ## 151       Trinidad-Tobago 0.11503360 0.48167080 0.12704479         NA
    ## 152               Tunisia 0.40441377         NA 0.12628432 0.15040533
    ## 153                Turkey 0.63361169         NA         NA 0.11608054
    ## 154          Turkmenistan 0.61493178         NA         NA 0.08013508
    ## 155                   UAE 0.24941330 0.13020370 0.15876747         NA
    ## 156                Uganda         NA 0.16328015 0.28439455         NA
    ## 157               Ukraine 0.66799351 0.05601664         NA 0.08771963
    ## 158               Uruguay 0.49441801         NA         NA         NA
    ## 159                   USA 0.15765677 0.32481876 0.11924459         NA
    ## 160            Uzbekistan 0.60421599         NA         NA 0.06953879
    ## 161             Venezuela 0.66588832         NA         NA         NA
    ## 162               Vietnam 0.77576670         NA         NA         NA
    ## 163                 Yemen 0.79083108         NA 0.06074801 0.03127973
    ## 164                Zambia 0.08265720 0.20160823 0.27868734         NA
    ## 165              Zimbabwe         NA 0.17262765 0.35564264 0.08914847
    ##        Italian
    ## 1           NA
    ## 2   0.11014675
    ## 3           NA
    ## 4           NA
    ## 5           NA
    ## 6           NA
    ## 7   0.12510398
    ## 8           NA
    ## 9           NA
    ## 10          NA
    ## 11          NA
    ## 12          NA
    ## 13          NA
    ## 14          NA
    ## 15          NA
    ## 16          NA
    ## 17          NA
    ## 18          NA
    ## 19          NA
    ## 20          NA
    ## 21          NA
    ## 22          NA
    ## 23          NA
    ## 24          NA
    ## 25          NA
    ## 26          NA
    ## 27          NA
    ## 28          NA
    ## 29          NA
    ## 30          NA
    ## 31          NA
    ## 32          NA
    ## 33  0.07195071
    ## 34          NA
    ## 35          NA
    ## 36          NA
    ## 37          NA
    ## 38          NA
    ## 39  0.12401819
    ## 40          NA
    ## 41          NA
    ## 42          NA
    ## 43          NA
    ## 44          NA
    ## 45          NA
    ## 46          NA
    ## 47  0.04881865
    ## 48  0.06842709
    ## 49          NA
    ## 50  0.06527315
    ## 51          NA
    ## 52          NA
    ## 53          NA
    ## 54          NA
    ## 55          NA
    ## 56          NA
    ## 57          NA
    ## 58          NA
    ## 59          NA
    ## 60          NA
    ## 61          NA
    ## 62          NA
    ## 63          NA
    ## 64  0.05867090
    ## 65          NA
    ## 66          NA
    ## 67          NA
    ## 68  0.06060465
    ## 69          NA
    ## 70          NA
    ## 71          NA
    ## 72          NA
    ## 73          NA
    ## 74          NA
    ## 75          NA
    ## 76          NA
    ## 77  0.08998663
    ## 78          NA
    ## 79          NA
    ## 80          NA
    ## 81          NA
    ## 82          NA
    ## 83          NA
    ## 84          NA
    ## 85          NA
    ## 86          NA
    ## 87          NA
    ## 88          NA
    ## 89          NA
    ## 90          NA
    ## 91          NA
    ## 92          NA
    ## 93          NA
    ## 94          NA
    ## 95          NA
    ## 96          NA
    ## 97  0.17853442
    ## 98          NA
    ## 99          NA
    ## 100 0.05682819
    ## 101         NA
    ## 102 0.13912486
    ## 103         NA
    ## 104 0.14288945
    ## 105         NA
    ## 106         NA
    ## 107         NA
    ## 108         NA
    ## 109         NA
    ## 110         NA
    ## 111         NA
    ## 112         NA
    ## 113         NA
    ## 114         NA
    ## 115         NA
    ## 116         NA
    ## 117         NA
    ## 118         NA
    ## 119         NA
    ## 120         NA
    ## 121         NA
    ## 122         NA
    ## 123         NA
    ## 124         NA
    ## 125         NA
    ## 126         NA
    ## 127         NA
    ## 128         NA
    ## 129         NA
    ## 130         NA
    ## 131         NA
    ## 132         NA
    ## 133         NA
    ## 134         NA
    ## 135 0.12093649
    ## 136         NA
    ## 137         NA
    ## 138         NA
    ## 139         NA
    ## 140         NA
    ## 141         NA
    ## 142         NA
    ## 143         NA
    ## 144         NA
    ## 145         NA
    ## 146         NA
    ## 147         NA
    ## 148         NA
    ## 149         NA
    ## 150         NA
    ## 151         NA
    ## 152         NA
    ## 153         NA
    ## 154         NA
    ## 155         NA
    ## 156         NA
    ## 157         NA
    ## 158 0.10546113
    ## 159         NA
    ## 160         NA
    ## 161 0.08034416
    ## 162         NA
    ## 163         NA
    ## 164         NA
    ## 165         NA

``` r
#pivot to create language variable
toplang_long <- toplang %>%
  pivot_longer(cols=c(2:6), names_to = "Language", values_to = "Percentage")

toplang_long
```

    ## # A tibble: 825 x 3
    ##    country     Language Percentage
    ##    <chr>       <chr>         <dbl>
    ##  1 Afghanistan English       0.189
    ##  2 Afghanistan Spanish       0.145
    ##  3 Afghanistan French       NA    
    ##  4 Afghanistan German        0.122
    ##  5 Afghanistan Italian      NA    
    ##  6 Albania     English       0.131
    ##  7 Albania     Spanish      NA    
    ##  8 Albania     French       NA    
    ##  9 Albania     German        0.327
    ## 10 Albania     Italian       0.110
    ## # ... with 815 more rows

``` r
#assign subgroups alphabetically in order to split plot for easier viewing
toplang_grouped <- toplang_long %>% 
  arrange(country) %>%
  mutate(counter = 1 + cumsum(as.integer(factor(country))), 
         # this counter starting at 1 increments for each country
         subgroup = as.factor(ceiling(counter/42)))

#check this
summary(toplang_grouped$subgroup)
```

    ##       1       2       3       4       5       6       7       8      10       9 
    ##      17       9       6       6       5       4       4       4       4       3 
    ##      11      12      13      14      16      18      20      15      17      19 
    ##       3       3       3       3       3       3       3       2       2       2 
    ##      21      22      23      24      25      26      27      28      29      30 
    ##       2       2       2       2       2       2       2       2       2       2 
    ##      31      32      33      35      36      37      39      40      42      44 
    ##       2       2       2       2       2       2       2       2       2       2 
    ##      46      48      50      52      54      57      60      63      67      71 
    ##       2       2       2       2       2       2       2       2       2       2 
    ##      76      82      92      34      38      41      43      45      47      49 
    ##       2       2       2       1       1       1       1       1       1       1 
    ##      51      53      55      56      58      59      61      62      64      65 
    ##       1       1       1       1       1       1       1       1       1       1 
    ##      66      68      69      70      72      73      74      75      77      78 
    ##       1       1       1       1       1       1       1       1       1       1 
    ##      79      80      81      83      84      85      86      87      88      89 
    ##       1       1       1       1       1       1       1       1       1       1 
    ##      90      91      93      94      95      96      97      98      99 (Other) 
    ##       1       1       1       1       1       1       1       1       1     624

``` r
#define a function to create plots
makeplot<-function(s) {
  toplang_grouped %>%
    filter(subgroup == s) -> df
  p1 <-ggplot(df,aes(x = Language, y = country, size=Percentage)) + geom_count() + labs(x="",y="") + theme(axis.text = element_text(angle = 45))+ scale_y_discrete(label = function(x) stringr::str_trunc(x, 12))
  if (s<4) {
    p1<-p1 + theme(legend.position="none")
  }
  if (s>1) {
    p1<-p1 
  }
  print(p1)
}

#add to a list
myplots<-lapply(
  1:4,makeplot
)
```

    ## Warning: Removed 6 rows containing missing values (geom_point).

![](2022-01-18_duolingo_KR_files/figure-gfm/create%20plots%20with%20function-1.png)<!-- -->

    ## Warning: Removed 4 rows containing missing values (geom_point).

![](2022-01-18_duolingo_KR_files/figure-gfm/create%20plots%20with%20function-2.png)<!-- -->

    ## Warning: Removed 3 rows containing missing values (geom_point).

![](2022-01-18_duolingo_KR_files/figure-gfm/create%20plots%20with%20function-3.png)<!-- -->

    ## Warning: Removed 1 rows containing missing values (geom_point).

![](2022-01-18_duolingo_KR_files/figure-gfm/create%20plots%20with%20function-4.png)<!-- -->

``` r
#arrange plots into one figure
bigplot <- ggpubr::ggarrange(plotlist=myplots, ncol=4, widths = c(1,1,1,1.25) )
```

    ## Warning: Removed 6 rows containing missing values (geom_point).

    ## Warning: Removed 4 rows containing missing values (geom_point).

    ## Warning: Removed 3 rows containing missing values (geom_point).

    ## Warning: Removed 1 rows containing missing values (geom_point).

``` r
bigplot
```

![](2022-01-18_duolingo_KR_files/figure-gfm/create%20plots%20with%20function-5.png)<!-- -->

``` r
#will only display correctly in full screen
```

``` r
#take language names (used gui to insert quotes, then edited manually to get country names)
times_ranked %>%
  toString() %>%
  print()
```

    ## [1] "country, English, Spanish, French, German, Italian, Japanese, Portuguese, Korean, Chinese, Russian, Turkish, Swedish, Danish, Swahili, Irish, Hindi, Norwegian, Arabic, Dutch, Finnish, Greek, Guarani, Hebrew, Vietnamese"

``` r
#subset by these countries
duo_countries_top_lang<-subset(toplang_long,country %in% c ("Great-Britain","Spain","France","Germany","Italy","Japan","Portugal","Korea","China","Russia","Turkey","Sweden","Denmark","Tanzania","Ireland","India","Norway","Egypt","Netherlands","Finnland","Greece","Guarana","Israel","Vietnam"))
```

``` r
plot2<- ggplot (duo_countries_top_lang, aes(Language,country, size=Percentage)) + geom_count()

plot2
```

    ## Warning: Removed 57 rows containing missing values (geom_point).

![](2022-01-18_duolingo_KR_files/figure-gfm/plot%20just%20these-1.png)<!-- -->

``` r
world <- ne_countries(scale = "medium", returnclass ="sf")

DuoMap <- merge(toplang,world, by.x="country", by.y="name", all=TRUE)

plot1<- ggplot(data = DuoMap) + geom_sf(aes(fill=English, size=.1, geometry=geometry))+ scale_size_identity()+
  ggtitle(paste("Popularity of","English","Amongst Duolingo Users", sep=" "), subtitle = "By Percentage of Total Users in Country")+
  theme_light()+
theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.title.y =element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_fill_distiller(palette="Blues", trans='reverse',na.value="transparent")

plot2<- ggplot(data = DuoMap) + geom_sf(aes(fill=Spanish, size=.1, geometry=geometry))+ scale_size_identity()+
  ggtitle(paste("Popularity of","Spanish","Amongst Duolingo Users", sep=" "), subtitle = "By Percentage of Total Users in Country")+
  theme_light()+
theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.title.y =element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_fill_distiller(palette="Greens", trans='reverse',na.value="transparent")

plot3<- ggplot(data = DuoMap) + geom_sf(aes(fill=French, size=.1, geometry=geometry))+ scale_size_identity()+
  ggtitle(paste("Popularity of","French","Amongst Duolingo Users", sep=" "), subtitle = "By Percentage of Total Users in Country")+
  theme_light()+
theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.title.y =element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_fill_distiller(palette="Reds", trans='reverse',na.value="transparent")

plot4<- ggplot(data = DuoMap) + geom_sf(aes(fill=German, size=.1, geometry=geometry))+ scale_size_identity()+
  ggtitle(paste("Popularity of","German","Amongst Duolingo Users", sep=" "), subtitle = "By Percentage of Total Users in Country")+
  theme_light()+
theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.title.y =element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_fill_distiller(palette="Purples", trans='reverse',na.value="transparent")


plot5<- ggplot(data = DuoMap) + geom_sf(aes(fill=Italian, size=.1, geometry=geometry))+ scale_size_identity()+
  ggtitle(paste("Popularity of","Italian","Amongst Duolingo Users", sep=" "), subtitle = "By Percentage of Total Users in Country")+
  theme_light()+
theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.title.y =element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_fill_distiller(palette="Oranges", trans='reverse',na.value="transparent")

plot1
```

![](2022-01-18_duolingo_KR_files/figure-gfm/try%20some%20mapping!-1.png)<!-- -->

``` r
plot2
```

![](2022-01-18_duolingo_KR_files/figure-gfm/try%20some%20mapping!-2.png)<!-- -->

``` r
plot3
```

![](2022-01-18_duolingo_KR_files/figure-gfm/try%20some%20mapping!-3.png)<!-- -->

``` r
plot4
```

![](2022-01-18_duolingo_KR_files/figure-gfm/try%20some%20mapping!-4.png)<!-- -->

``` r
plot5
```

![](2022-01-18_duolingo_KR_files/figure-gfm/try%20some%20mapping!-5.png)<!-- -->

``` r
ggsave("plot1.png", plot = plot1)
```

    ## Saving 7 x 5 in image

``` r
ggsave("plot2.png", plot = plot2)
```

    ## Saving 7 x 5 in image

``` r
ggsave("plot3.png", plot = plot3)
```

    ## Saving 7 x 5 in image

``` r
ggsave("plot4.png", plot = plot4)
```

    ## Saving 7 x 5 in image

``` r
ggsave("plot5.png", plot = plot5)
```

    ## Saving 7 x 5 in image
