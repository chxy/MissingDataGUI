##' A Graphical User Interface for Exploring Missing Values in Data
##'
##' This package was designed mainly for the exploration of
##' missing values structure, and results of imputation, using static
##' graphics and numerical summaries. A graphical user interface (GUI)
##' makes it accessible to novice users.
##'
##' @name MissingDataGUI-package
##' @docType package
##' @importFrom grDevices dev.off png
##' @importFrom stats na.omit as.formula complete.cases dist hclust median rnorm runif
##' @references Xiaoyue Cheng, Dianne Cook, Heike Hofmann (2015). Visually Exploring Missing Values in Multivariable Data Using a Graphical User Interface. Journal of Statistical Software, 68(6), 1-23. doi:10.18637/jss.v068.i06
##' @examples
##' if (interactive()) {
##' MissingDataGUI()
##' }
##'
NULL


##' West Pacific Tropical Atmosphere Ocean Data, 1993 & 1997.
##'
##' Real-time data from moored ocean buoys for improved detection,
##' understanding and prediction of El Ni'o and La Ni'a.
##'
##' The data is collected by the Tropical Atmosphere Ocean project (
##' \url{http://www.pmel.noaa.gov/tao/index.shtml}).
##'
##' Format: a data frame with 736 observations on the following 8
##' variables. \describe{\item{\code{year}}{A factor with levels
##' \code{1993} \code{1997}.} \item{\code{latitude}}{A factor with
##' levels \code{-5}  \code{-2} \code{0}.} \item{\code{longitude}}{A
##' factor with levels \code{-110} \code{-95}.}
##' \item{\code{sea.surface.temp}}{Sea surface temperature(degree
##' Celsius), measured by the TAO buoys at one meter below the
##' surface.}  \item{\code{air.temp}}{Air temperature(degree Celsius),
##' measured by the TAO buoys three meters above the sea surface.}
##' \item{\code{humidity}}{Relative humidity(%), measured by the TAO
##' buoys 3 meters above the sea surface.} \item{\code{uwind}}{The
##' East-West wind vector components(M/s).  TAO buoys measure the wind
##' speed and direction four meters above the sea surface. If it is
##' positive, the East-West component of the wind is blowing towards
##' the East. If it is negative, this component is blowing towards the
##' West.}\item{\code{vwind}}{The North-South wind vector
##' components(M/s). TAO buoys measure the wind speed and direction
##' four meters above the sea surface. If it is positive, the
##' North-South component of the wind is blowing towards the North.
##' If it is negative, this component is blowing towards the South.}}
##' @name tao
##' @docType data
##' @usage data(tao)
##' @source \url{http://www.pmel.noaa.gov/tao/data_deliv/deliv.html}
##' @keywords datasets
##' @examples
##' if (interactive()) {
##' data(tao)
##' MissingDataGUI(tao)
##' }
##'
NULL


##' The Behavioral Risk Factor Surveillance System (BRFSS) Survey
##' Data, 2009.
##'
##' The data is a subset of the 2009 survey from BRFSS, an ongoing
##' data collection program designed to measure behavioral risk
##' factors for the adult population (18 years of age or older) living
##' in households.
##'
##' Also see the codebook:
##' \url{http://ftp.cdc.gov/pub/data/brfss/codebook_09.rtf}
##'
##' Format: a data frame with 245 observations on the following 34
##' variables. \describe{\item{\code{STATE}}{A factor with 52 levels.
##' The labels and states corresponding to the labels are as follows.
##' 1:Alabama, 2:Alaska, 4:Arizona, 5:Arkansas, 6:California,
##' 8:Colorado, 9:Connecticut, 10:Delaware, 11:District of Columbia,
##' 12:Florida, 13:Georgia, 15:Hawaii, 16:Idaho, 17:Illinois,
##' 18:Indiana, 19:Iowa, 20:Kansas, 21:Kentucky, 22:Louisiana,
##' 23:Maine, 24:Maryland, 25:Massachusetts, 26:Michigan,
##' 27:Minnesota, 28:Mississippi, 29:Missouri, 30:Montana,
##' 31:Nebraska, 32:Nevada, 33:New Hampshire, 34:New Jersey, 35:New
##' Mexico, 36:New York, 37:North Carolina, 38:North Dakota, 39:Ohio,
##' 40:Oklahoma, 41:Oregon, 42:Pennsylvania, 44:Rhode Island, 45:South
##' Carolina, 46:South Dakota, 47:Tennessee, 48:Texas, 49:Utah,
##' 50:Vermont, 51:Virginia, 53:Washington, 54:West  Virginia,
##' 55:Wisconsin, 56:Wyoming, 66:Guam, 72:Puerto Rico, 78:Virgin
##' Islands} \item{\code{SEX}}{A factor with levels \code{Male}
##' \code{Female}.} \item{\code{AGE}}{A numeric vector from 7 to 97.}
##' \item{\code{HISPANC2}}{A factor with levels \code{Yes} \code{No}
##' corresponding to the question: are you Hispanic or Latino?}
##' \item{\code{VETERAN2}}{A factor with levels \code{1} \code{2}
##' \code{3} \code{4} \code{5}. The question for this variable is:
##' Have you ever served on active duty in the United States Armed
##' Forces, either in the regular military or in a National Guard or
##' military reserve unit? Active duty does not include training for
##' the Reserves or National Guard, but DOES include activation, for
##' example, for the Persian Gulf War. And the labels are meaning: 1:
##' Yes, now on active duty; 2: Yes, on active duty during the last 12
##' months, but not now; 3: Yes, on active duty in the past, but not
##' during the last 12 months; 4: No, training for Reserves or
##' National Guard only; 5: No, never served in the military.}
##' \item{\code{MARITAL}}{A factor with levels \code{Married}
##' \code{Divorced} \code{Widowed} \code{Separated}
##' \code{NeverMarried} \code{UnmarriedCouple}.}
##' \item{\code{CHILDREN}}{A numeric vector giving the number of
##' children less than 18 years of age in household.}
##' \item{\code{EDUCA}}{A factor with the education levels \code{1}
##' \code{2} \code{3} \code{4} \code{5} \code{6} as 1: Never attended
##' school or only kindergarten; 2: Grades 1 through 8 (Elementary);
##' 3: Grades 9 through 11 (Some high school); 4: Grade 12 or GED
##' (High school graduate); 5: College 1 year to 3 years (Some college
##' or technical school); 6: College 4 years or more  (College
##' graduate).} \item{\code{EMPLOY}}{A factor showing the employment
##' status with levels \code{1} \code{2} \code{3} \code{4} \code{5}
##' \code{7} \code{8}. The labels mean --  1: Employed for wages; 2:
##' Self-employed; 3: Out of work for  more than 1 year; 4: Out of
##' work for less that 1 year;  5: A homemaker; 6: A student; 7:
##' Retired; 8: Unable to work.}  \item{\code{INCOME2}}{The annual
##' household income from all sources with levels \code{<10k}
##' \code{10-15k} \code{15-20k} \code{20-25k} \code{25-35k}
##' \code{35-50k} \code{50-75k} \code{>75k} \code{Dontknow}
##' \code{Refused}.} \item{\code{WEIGHT2}}{The weight without shoes in
##' pounds.} \item{\code{HEIGHT3}}{The weight without shoes in
##' inches.} \item{\code{PREGNANT}}{Whether pregnant now with two
##' levels \code{Yes} and \code{No}.}  \item{\code{GENHLTH}}{The
##' answer to the question "in general your health is" with levels
##' \code{Excellent} \code{VeryGood} \code{Good} \code{Fair}
##' \code{Poor} \code{Refused}.}  \item{\code{PHYSHLTH}}{The number of
##' days during the last 30 days that the respondent's physical health
##' was not good.  -7 is for "Don't know/Not sure", and -9 is for
##' "Refused".}  \item{\code{MENTHLTH}}{The number of days during the
##' last 30 days that the respondent's mental health was not good.  -7
##' is for "Don't know/Not sure", and -9 is for "Refused".}
##' \item{\code{POORHLTH}}{The number of days during the last 30 days
##' that poor physical or mental health keep the respondent from doing
##' usual activities, such as self-care, work, or recreation. -7 is
##' for "Don't know/Not sure", and -9 is for "Refused".}
##' \item{\code{HLTHPLAN}}{Whether having any kind of health care
##' coverage, including health insurance, prepaid plans such as HMOs,
##' or government plans such as Medicare. The answer has two levels:
##' \code{Yes} and \code{No}.}  \item{\code{CAREGIVE}}{Whether
##' providing any such care or assistance to a friend or family member
##' during the past month, with levels \code{Yes} and \code{No}.}
##' \item{\code{QLACTLM2}}{ Whether being limited in any way in any
##' activities because of physical, mental, or emotional problems,
##' with levels  \code{Yes} and \code{No}.}
##' \item{\code{DRNKANY4}}{Whether having had at least one drink of
##' any alcoholic beverage such as beer, wine, a malt beverage or
##' liquor during the past 30 days, with levels \code{Yes} and
##' \code{No}.}  \item{\code{ALCDAY4}}{The number of days during the
##' past 30 days that the respondent had at least one drink of any
##' alcoholic beverage. -7 is for "Don't know/Not sure", and -9 is
##' for "Refused".} \item{\code{AVEDRNK2}}{The number of drinks on the
##' average the respondent had on the days when he/she drank, during
##' the past 30 days. -7 is for "Don't know/Not sure", and -9 is for
##' "Refused".} \item{\code{SMOKE100}}{ Whether having smoked at least
##' 100 cigarettes in the entire life, with levels \code{Yes} and
##' \code{No}.} \item{\code{SMOKDAY2}}{ The frequency of days now
##' smoking, with levels \code{Everyday} \code{Somedays} and
##' \code{NotAtAll}(not at all).}  \item{\code{STOPSMK2}}{Whether
##' having stopped smoking for  one day or longer during the past 12
##' months because the respondent was trying to quit smoking, with
##' levels \code{Yes} and \code{No}.} \item{\code{LASTSMK1}}{A factor
##' with levels \code{3} \code{4} \code{5} \code{6} \code{7} \code{8}
##' corresponding to the question: how long has it been since last
##' smokeing cigarettes regularly? The labels mean: 3: Within the past
##' 6 months (3 months but less than 6 months ago); 4: Within the past
##' year (6 months but less than 1 year ago); 5: Within the past 5
##' years (1 year but less than 5 years ago); 6: Within the past 10
##' years (5 years but less than 10 years ago); 7: 10 years or more;
##' 8: Never smoked regularly.} \item{\code{FRUIT}}{The number of
##' fruit the respondent eat every year, not counting juice. -7 is for
##' "Don't know/Not sure", and -9 is for "Refused".}
##' \item{\code{GREENSAL}}{The number of servings of green   salad the
##' respondent eat every year. -7 is for "Don't  know/Not sure",
##' and -9 is for "Refused".} \item{\code{POTATOES}}{ The number of
##' servings of potatoes, not including french fries, fried potatoes,
##' or potato chips, that the respondent eat every year. -7 is for
##' "Don't know/Not sure", and -9 is for "Refused".}
##' \item{\code{CARROTS}}{The number of carrots the respondent eat
##' every year. -7 is for  "Don't know/Not sure", and -9 is for
##' "Refused".}  \item{\code{VEGETABL}}{The number of servings of
##' vegetables   the respondent eat every year, not counting carrots,
##' potatoes, or salad. -7 is for "Don't know/Not sure", and -9 is
##' for "Refused".} \item{\code{FRUITJUI}}{The number of fruit juices
##' such as orange, grapefruit, or tomato that the respondent drink
##' every year. -7 is   for "Don't know/Not sure", and -9 is for
##' "Refused".}  \item{\code{BMI4}}{Body Mass Index (BMI). Computed by
##' WEIGHT in Kilograms/(HEIGHT in Meters * HEIGHT3 in Meters).
##' Missing if any of WEIGHT2 or HEIGHT3 is missing.} }
##' @name brfss
##' @docType data
##' @usage data(brfss)
##' @source \url{http://www.cdc.gov/brfss/data_documentation/index.htm}
##' @keywords datasets
##' @examples
##' if (interactive()) {
##' data(brfss)
##' MissingDataGUI(brfss)
##' }
##'
NULL
