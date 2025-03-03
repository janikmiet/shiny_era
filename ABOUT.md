## Exposure Response Analysis

This tool is used for exploring population health with certain diagnoses. For example we want to explore sleep apnea patients (ICD-10 code G47) and what other common diagnoses they have. Then we can select response diagnoses, for example hypertension (ICD-10 code I10), and explore how sleep apnea patients will get this diagnose during the time. 

Use regex code (ex. ^G47 | ^G31) to select diagnose groups. You can also select which registries are used as a source. Application is logging user input and runs. 

**If selected group size is under 6 person, results are not printed and error occur.** 


## Useful links:

- [ICD-10 Codes (Terveysportti)](https://www.terveysportti.fi/apps/icd/) 
- [ICD-10 Codes (Koodistopalvelu)](https://koodistopalvelu.kanta.fi/codeserver/pages/classification-view-page.xhtml?classificationKey=23) 
- [ICD-9 Codes](https://www.julkari.fi/handle/10024/131850)
- [ICD-8 Codes](https://www.julkari.fi/handle/10024/135324)
- [regex cheatsheet](https://hypebright.nl/index.php/en/2020/05/25/ultimate-cheatsheet-for-regex-in-r-2/)

## Changes

| Date       | Version    |  Description | 
| :--------- | :--------- | :--------------------------------------------------------------------- | 
| 2025/03/03 | 1.5.9 | Progression bars are added to the application to help to know what app is doing. | 
| 2025/02/05 | 1.5.7 | Added synthetic data. All other but Poisson results works with it (this is why report download does not work). Data does not describe real world case. Only Shows how app works. | 
| 2025/01/18 | 1.5.6 | Report added. Still lot of adjustment needs be done. | 
| 2024/11/18 | 1.5.5 | First version of ERA Shiny | 