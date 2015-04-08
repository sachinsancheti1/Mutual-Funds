# Mutual-Funds
An extract of Mutual Funds data available in India from the economictimes mutual funds website. The main functions are the scraping and the running of the report from the data extracted

##Scraping

Use the *scraping.R* to extract data out from the internet. The functions as well as the running of the program are available within this file.

Data scraped, is verified again for any errors, and rescraped if necessary. This function is especially useful if the internet gets cut over time.
If there is a failure, rerun the section on verification to fix the error.

##Report

All data, after scraping is placed on the 'custom named' *.Rdata* file. Once the data is placed here, run the report using the *MutualFunds-analysis.Rmd*.

##Sample Published report

You can find a sample published report on [this link](https://sachinsancheti.shinyapps.io/Mutual-Funds/MutualFunds-analysis.Rmd)

## Bugs, fixes and performance improvements

Will be glad if others could also contribute to improve performance, get better data or empower investors in any way possible to make better decisions
