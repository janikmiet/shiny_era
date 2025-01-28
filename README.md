## Exposure Response Analysis

This tool is used for exploring population health with certain diagnoses. For example we want to explore sleep apnea patients (ICD-10 code G47) and what other common diagnoses they have. Then we can select response diagnoses, for example hypertension (ICD-10 code I10), and explore how sleep apnea patients will get this diagnose during the time. 

Use regex code (ex. ^G47 | ^G31) to select diagnose groups. You can also select which registries are used as a source. Application is logging user input and runs. **If selected group size is under 6 person, results are not printed and error occur.** 


### Common Data model

#### population

| VARIABLE       | DESCRIPTION | 
| :--------- | :--------------------------------------------------------------------- | 
| LOMNO1 | person ID variable  | 
| DATE_BIRTH | Date of person birthday | 
| DATE_DEATH | Date of person death date or NA  | 
| DATE_MIGRATION | Date of person migration or NA  | 

#### diagnoses

| VARIABLE       | DESCRIPTION | 
| :--------- | :--------------------------------------------------------------------- | 
| LOMNO1 | person ID variable | 
| DGREG | Information of diagnose registry. Example ICD10, ICD9 or ICD8 or FRACTURES registry | 
| SRC |  Diagnose registry source. Example Hilmo, Cancer etc. healthcare registry |  
| DATE | Date of event  | 
| DG | Diagnose code  |  
| ICD10_3LETTERS | NA or in case of ICD10 code, icd code in 3 letters  | 
| ICD10_CLASS | | NA or in case of ICD10 code, icd class (upper definition)  | 
| DATE_BIRTH | Person date of birth | 
| AGE | Age of the event | 


### Changes

| Date       | Version    |  Description | 
| :--------- | :--------- | :--------------------------------------------------------------------- | 
| 2025/11/18 | 1.5.6 | Report added. Still lot of adjustment needs be done. | 
| 2024/11/18 | 1.5.5 | First version of ERA Shiny | 



### Useful links:

- [ICD-10 Codes (Terveysportti)](https://www.terveysportti.fi/apps/icd/) 
- [ICD-10 Codes (Koodistopalvelu)](https://koodistopalvelu.kanta.fi/codeserver/pages/classification-view-page.xhtml?classificationKey=23) 
- [ICD-9 Codes](https://www.julkari.fi/handle/10024/131850)
- [ICD-8 Codes](https://www.julkari.fi/handle/10024/135324)
- [regex cheatsheet](https://hypebright.nl/index.php/en/2020/05/25/ultimate-cheatsheet-for-regex-in-r-2/)