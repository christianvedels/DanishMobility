# DanishMobility

INSERT DESCRIPTION
HERE IS A COMMIT

HERE IS ANOTHER COMMIT

[**Slides**](https://raw.githack.com/christianvedels/DanishMobility/main/Project_dissemination/DanishMobility_slides/Slides.html)

# Data
### A. Census data
From Link Lives (Robinson et al, 2022), https://www.rigsarkivet.dk/udforsk/link-lives
-data/. This is enriched with HISCO codes using a the procedure from Dahl, Johansen, Vedel (2024) - see below. The data is located in the dropbox data folder Data/Link_lives. See link lives guides for documentation of 'standardized_sources' and 'transcribed_sources'

### B. HISCO codes for census data
The HISCO census data was enriched with HISCO codes from OccCANINE with a threshold of 0.11, which is optimal for Danish sources. 200 random observations were checked to have the correct HISCO code in 95.0 percent of cases. The data is located in Data/Link_lives

| Variable                | Description                                                                                         |
| ----------------------- | --------------------------------------------------------------------------------------------------- |
| `pa_id`                 | Unique identifier for the person from Link Lives                                                    |
| `Kilde`                 | Source of the information from Link Lives                                                           |
| `Erhverv`               | Profession or trade of the individual from Link Lives                                               |
| `Stilling_i_husstanden` | Position or status within the household from Link Lives (sometimes this contains occupational info) |
| `occ1`                  | Merge of `Erhverv` and `Stilling_i_husstanden`                                                      |
| `inputs`                | Occupational description, which was fed to OccCANINE                                                |
| `hisco_[x]`             | [x]th HISCO (Historical International Standard Classification of Occupations) code                  |
| `prob_[x]`              | Model confidence level of `hisco_[x]` being correct                                                 |
| `desc_[x]`              | Description of `hisco_[x]`                                                                          |


# Updates
initials (date)
- CV (2024-03-21): Added Link lives data + HISCO codes + Documentation
- CV (2024-03-05): Made repo

## References
Dahl, C. M., Johansen, T. S. D., Vedel, C. (2024). Breaking the HISCO barrier: Automatic occupational standardization with OccCANINE. _arXiv preprint_ arXiv:2402.13604. https://arxiv.org/abs/2402.13604

Robinson, O., Mathiesen, N. R., Thomsen, A. R., & Revuelta-Eugercios, B. (2022, Jun). Link-lives
guide: version 1. Online. (Available at: https://www.rigsarkivet.dk/udforsk/link-lives
-data/)


