## Do Flight Delays Increase the Likelihood That Your Baggage Will Be Mishandled?

#### Background: If you’re one of the millions of people in the U.S. who travel by airline, then you’re probably no stranger to one, if not both of these persistent issues: flight delays and the mishandling of your baggage. Flight delays can leave you languishing in airports, missing connecting flights and ruining plans, while baggage mishandling can strand you without essentials or treasured possessions. Ultimately, both delays and mishandling cost the customer time and money, undermining the travel experience. Despite its prevalence, the relationship between flight delays and baggage mishandling does not appear to have been well-explored. These seemingly unrelated and highly problematic occurrences may in fact be connected in ways that are not immediately apparent. This project will investigate the relationship between these two issues with the goal of uncovering patterns or correlations. In doing so, we hope to provide information that will allow the customer to make more informed decisions about their travel plans and prepare for potential disruptions, potentially mitigating considerable stress and unexpected costs.

#### The proposed analysis will utilize flight data, including records of delays and cancellations,  The analysis will be conducted using R, with the predictor variable being Delay Time/Cancellation. Potential feature variables include flight date, cancellation status, delay times, and mishandled baggage reported.

#### Research Question: Do Flight Delays Increase the Likelihood That Your Baggage Will Be Mishandled?

#### Hypothesis: Baggage on flights that have been delayed or canceled is more likely to be mishandled. We therefore expect a significant positive correlation between these variables.

#### Predictions:  For every 15 minutes a flight is delayed, there is a 5% increase in the likelihood that baggage will be mishandled. If a flight is canceled, the risk increases by 20%.

## Setup 

To run this project, follow these steps:

1. *Clone the repo* to your environment.
2. *Download the datasets*. The datasets are too large for GitHub's standard 50Mb limit. They can be [downloaded here](https://drive.google.com/drive/folders/1PZoEmxvO38UAgzIgKhfDqfICzB_NzIGV?usp=sharing).
3. *Move datasets to the `data/` directory and unzip.
4. *Set `PROJ_PATH` in `/scripts/main.R`*. This project path variable should point to the root directory of the repo in your environment.
4. *Run `/scripts/main.R`*. This script performs all data processing and modeling in sequence -- it takes a while.

***Notes***: 

- Each script can be run independently but currently you need to set `PROJ_PATH` individually in each script.
- All scripts are called by `main.R` except `/scripts/data-merge-csvs-online` and `scripts/prelim_eda.R`
- `/scripts/data-merge-csvs-online` merges large .csvs and will not run - it depends on a large number of large datasets which are currently not available at the Drive link above.
- `scripts/prelim_eda.R` generates EDA plots.


