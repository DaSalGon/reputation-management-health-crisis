# reputation-management-health-crisis
Replication material for Varela Castro, Bustos &amp; Saldivia Gonzatti (2023). Reputation management during a public health crisis: Overcompensating when all else fails. PAR, Online first. [https://onlinelibrary.wiley.com/doi/10.1111/puar.13638]

This repository contains code to reproduce the analyses in Varela Castro, Bustos &amp; Saldivia Gonzatti (2023).

The data folder includes two unique text-datasets based on public information from the Mexican Secretariat of Health: (1) the Press Conference dataset covering Covid-19 related conferences from 2020 and 2021 (N days = 449; N intervention = 10.813); and (2) the Press Releases dataset convering publications by the Secretariat from 2013 to 2022 (N = 6,196). Please, do refer to both datasets by citing the paper:

```
@article{https://doi.org/10.1111/puar.13638,
author = {Varela Castro, Samanta and Bustos, Edgar O. and Saldivia Gonzatti, Daniel},
title = {Reputation management during a public health crisis: Overcompensating when all else fails},
journal = {Public Administration Review},
volume = {n/a},
number = {n/a},
pages = {},
doi = {https://doi.org/10.1111/puar.13638},
url = {https://onlinelibrary.wiley.com/doi/abs/10.1111/puar.13638},
}
```
Do cite the paper when using the reputational dimension dictionary in Spanish (see data folder). Do refer to the original dicitionary in English by [Busuioc and Rimkuté (2020)](https://www.tandfonline.com/doi/full/10.1080/13501763.2019.1603248).

You can find a pre-print version of the paper [in ResearchGate](https://www.researchgate.net/publication/369767678_Reputation_Management_During_a_Public_Health_Crisis_The_COVID_-19_Pandemic_in_Mexico).

## Scripts

Both .rmd files replicate the analyses and results in the paper with the data available. Remaining R scripts on conference data generation, Twitter data access and press releases download serve to protocol the accesibility code to the raw data. Original tweets cannot be made available, therefore, we only share aggregated indicators. We can share tweet IDs upon request.

## Data

Beside the above mentioned data, you need to download the Spanish sentiment dictionary from the source. Refer to the Spanish sentiment dictionary by [Proksch et al. (2019)](https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/ALFLK6/2K6ZHD). Do refer to the original, non-translated [Lexicoder Sentiment Dictionary (LSD)](https://www.snsoroka.com/data-lexicoder/) by Daku, Young and Soroka.


