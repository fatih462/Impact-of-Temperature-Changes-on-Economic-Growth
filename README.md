# Impact of Temperature Changes on Economic Growth: A comparative Analysis of Wealthy and Poor Countries

In this project, I studied the impact of temperature on economic growth, with a focus on whether poorer countries are more vulnerable to rising temperatures than wealthier ones in the face of global warming.
I based my approach on the methodology of Dell et al. (2012). First, I replicated their work and then extended the period of analysis by an additional 11 years, resulting in approximately 37% more observations
As part of this, I also reviewed data revisions and corrections made by the World Bank, since updated GDP data slightly altered the original results of Dell et al. (2012).


## Project Structure

- **data/**: This folder contains all the datasets used for the analysis.
    - `dataset_by_Dell.dta`: This dataset was used and published by Dell et al. 2012, which we use to replicate their work.
    - `cl.rds`: This is the main dataset we use for everything else.
    - `capita_LCU.xlsx`: World bank WDI data on GDP per capita (constant LCU)
    - `capita_PPP.xlsx`: World bank WDI data on GDP per capita, PPP (constant 2021 international $)

- **figures/**: This folder contains figures used iin the paper.
    - `Figure1.png`
    - `Figure2.png`

- **code/**: This folder contains all the code written in R or Python for our analysis.
    - `Replication_of_Dell.R`: Replicates the main results of Dell et al. 2012.
    - `new_weighted_temp.ipynb`: Calculates population weighted average annual temperature between 1960 and 2014 for every country in our analysis.
    - `making_the_dataset.R`: Constructs our main dataset cl.rds by including new temperature data, new gdp data and some information from dataset_by_Dell.dta
    - `descriptive_statistics.R`: Calculates figures and other descriptive statistics.
    - `main_table.R`: Calculates the main results of our regression analysis.
    - `Robustness.R`: Contains Robustness Tests for our results.


## Installation & Usage

To run the analysis, you will need R installed, along with the necessary libraries. If you want to download the temperature data yourself (instead of using the data in cl.rds), you will need ArcGIS software that supports running Arcpy code.
