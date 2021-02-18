<img src="ndlbanner.png" width="405" height="96">

# Networked Data Lab: Morbidity and long-term conditions among those advised to shield from COVID-19

#### Project Status: In progess

## Project Description

- This Networked Data Lab analysis focusses on clinically extremely vulnerable (CEV) people, also known as the shielding population - the group of people most at risk of becoming seriously ill from COVID-19 . This group were asked to not leave their homes and to minimise all face-to-face contact up until the end of July 2020 in most of the UK. Whilst the shielding guidance was paused over Summer, people were once again asked to minimise their contact with others from November.

- We are using a federated approach to data analysis and each partner will be contributing the same descriptive analysis based on their local population. These results will then be analysed and aggregated where necessary.

- We will identify long-term conditions by looking back at hospital admission health care records in the period between 1 March 2018 and 29 February 2020. The long-term conditions identified for this analysis are the conditions which comprise the Elixhauser Comorbidity Index. In some cases, there may be overlap with the conditions on the
shielded patient list.

- We will aim to identify each of the following conditions by identifying inpatient records that match the relevant ICD-10 cides. In short, an individual is assumed to have had the condition if any record exists during this period matching the relevant ICD-10 codes corresponding to this condition (regardless of whether this was the primary diagnosis). A coding algorithm is set out by [Quan et al (2005)](https://pubmed.ncbi.nlm.nih.gov/21764557/).

## Partners

The following partners have taken part in this analysis and contributed results:

- The Aberdeen Centre for Health Data Science (ACHDS) which includes NHS Grampian and the University of Aberdeen
- Public Health Wales, NHS Wales Informatics Service (NWIS), Swansea University (SAIL Databank) and Social Care Wales (SCW)
- Imperial College Health Partners (ICHP), Institute of Global Health Innovation (IGHI), Imperial College London (ICL), and North West London CCGs
- Liverpool CCG, Healthy Wirral Partnership and Citizens Advice Bureau
- Leeds CCG and Leeds City Council    

## Data sources

This analysis relies on the following data sources, which have been accessed by NDL partners.

- The Shielded Patient List (SPL).
- Hospital admission records
- Patient demographics databases.
- External Open data sources linked to a patients’ LSOA (or other geography) of residence. These are the 2019 [English](https://data-communities.opendata.arcgis.com/datasets/d4b79be994ac4820ad44e10ded313df3_0
)/[Welsh](https://gov.wales/sites/default/files/statistics-and-research/2019-11/welsh-index-multiple-deprivation-2019-index-and-domain-ranks-by-small-area.ods
)/[Scottish](https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2020/01/scottish-index-of-multiple-deprivation-2020-data-zone-look-up-file/documents/scottish-index-of-multiple-deprivation-data-zone-look-up/scottish-index-of-multiple-deprivation-data-zone-look-up/govscot%3Adocument/SIMD%2B2020v2%2B-%2Bdatazone%2Blookup.xlsx) Index of Multiple Deprivation quintiles and [English](https://data.gov.uk/dataset/b1165cea-2655-4cf7-bf22-dfbd3cdeb242/rural-urban-classification-2011-of-lower-layer-super-output-areas-in-england-and-wales)/[Welsh](https://data.gov.uk/dataset/b1165cea-2655-4cf7-bf22-dfbd3cdeb242/rural-urban-classification-2011-of-lower-layer-super-output-areas-in-england-and-wales)/[Scottish](https://www.opendata.nhs.scot/fa_IR/dataset/urban-rural-classification) urban/rural indicators based on the 2011 Census.

### Requirements

These scripts were written in R version 4.0.2 and RStudio Version 1.1.383. 

## Authors

* Sebastien Peytrignet, The Health Foundation - [Twitter](https://twitter.com/SebastienPeytr2) - [GitHub](https://github.com/speytrignet-thf)
* Karen Hodgson, The Health Foundation - [Twitter](https://twitter.com/KarenHodgePodge) - [GitHub](https://github.com/KarenHodgson)

## License

This project is licensed under the [MIT License](https://github.com/HFAnalyticsLab/NDL_Output2_Morbidity/blob/main/LICENSE).
