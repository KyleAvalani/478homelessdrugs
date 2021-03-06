# Project Description
_A written, non-technical description of our project._

### What is the purpose of your research project?
The purpose of our project is to analyze the links between overdose and homeless populations in WA state. We will determine whether the conditions of homelessness (such as sheltered, unsheltered, and being a veteran) correlates to the prevalence of overdoses across each county.

### What other research has been done in this area? Make sure to include 3+ links to related works.

+ [Substance Abuse and Homelessness](https://nationalhomeless.org/wp-content/uploads/2017/06/Substance-Abuse-and-Homelessness.pdf) - This study was published by the National Coalition for the Homeless. The final result is meant to be informative with a mainly text and specific statistics published.
+ [Pathways to Homelessness among Older Homeless Adults: Results from the HOPE HOME Study](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4862628/?fbclid=IwAR1DppB1ZTi8VEMX3GD2BCbHRYsObr8XCudAszwO4_6zhktyfOTxO5H3Ofc#!po=68.3962) - This study tried to examine what led to homelessness for adults. The study looked at many factors which included drug use and alcohol use. This study is more broad while we hope to do something drug and alcohol specific.
+ [Seattle Homeless Needs Assessment](https://www.documentcloud.org/documents/3480319-City-of-Seattle-Homeless-Needs-Assessment-March.html) - From 2016 a general survey of the homeless conducted in Seattle. The survey includes graphics to understand various information on Seattle's homeless population. This survey includes a section on drug use.

### What is the dataset you'll be working with?  Please include background on who collected the data, where you accessed it, and any additional information we should know about how this data came to be.

We'll be working with two datasets.

The first is *'Washington State Residents Drug Overdose Quarterly Report'* for the year of 2018. This data is made public on the Washington State Department of Health website. The deaths and hospitalization data from this report are taken from the DOH’s Death Certificates and Comprehensive Hospital Abstract Reporting System (CHARS)

> [Link to report](https://www.doh.wa.gov/Portals/1/Documents/8300/wa_lhj_quarterly_report_18_1_2_pub.html)

The second dataset we're looking at is the Washington State Department of Commerce's *'2018 Point in Time Detailed County Results'*. This data details the conditions and prevalence of homelessness across Washington counties. By the 'Homeless Housing and Assistance Act', it is required for each county in Washington State to conduct an annual point-in-time count of sheltered and unsheltered homeless persons.

> [Link to dataset (Excel file)](http://www.commerce.wa.gov/wp-content/uploads/2013/01/hau-2018-pit-detailed-county-results.xlsx)

### Who is your target audience?  Depending on the domain of your data, there may be a variety of audiences interested in using the dataset. You should hone in on one of these audiences.

Our target audience is homeless assistance programs. Insight into the connection between overdoses and homelessness will allow assistance programs to better allocate their health resources.

### What should your audience learn from your resource? Please consider specific questions that your project can answer using the data you have identified (that are related to the overarching purpose)

From our resource, the audience will learn:

* Which kinds of homeless people are most affected by drug overdose? This will aid Homeless Assistance programs in knowing which category of homeless people to target for rehabilitation.
* Which types of drugs are the most prevalent in the overdoses of homeless people?
* Which types of drug overdoses are prevalent in each county?

# Technical Description
_A technical description of the tools and datasets we'll be using._

### What will be the format of your final product (Shiny app, HTML page, compiled .Rmd file, etc)?
We are going to be publishing our report as an interactive Shiny app. 

### Do you anticipate any specific data collection / data management challenges?
For now, we think we have collected enough data to analyze and gain insight from. If we find ourselves needing more data, it is certainly feasible to gather more.

We anticipate a challenge in combining our datasets into one, cohesive dataframe.

### What new technical skills will need to learn in order to complete your project?
We may need to learn how to use new libraries in order to clean our data or to create vizualizations to represent it. Specifically, if we choose to use a map in displaying data, we'll need to (re)learn leaflet. 

### What major challenges do you anticipate? 
It will be difficult to opitmize how our data is displayed. We must ensure that our interactive map is not too cluttered, but at the same time, shows enough data to actually be useful.
