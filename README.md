# Replication materials for "Measuring Transparency in the Social Sciences"

The following is a walk-through of the methods we used for the paper "Measuring Transparency in the Social Sciences." It is written in an informal style and goes into more detail than the paper allowed in order to explain the technical details and the specific code files that do the work for the project.

Much of the repository is messy. We beg the reader's clemency. The project was exploratory and iterative, and it was difficult to conceptualise at the beginning how the workflow could have been turned into an end-to-end pipeline while it was still being constructed. Future work in this area will not suffer the same deficit. Nevertheless, the following should give a sense for how the project came together and provide enough information for an interested reader to reconstruct our steps.

# Methods section from the paper (dated March 30, 2022)

Our study design called for a comprehensive analysis of population-level data. Our population --- (1) papers using data and statistics, and (2) original experiments --- were part of *all* political science and international relations publications in target journals. We downloaded all of the journals' papers from 2010 to July 2022. Once we had these papers, we identified the data, statistical, and experimental papers through dictionary-based feature engineering and machine learning. We then used public APIs, web scraping, and text analysis to identify which of the studies had replication materials. We outline this process below.

> Files: 
>	 - /ref/JCR_Political_Science.csv
>	 - /ref/JCR_International_Relations.csv
>	 
>	 These files were obtained directly from the Clarivate subscription-only website in August 2021.

## Phase one: gathering and classifying the papers

> Code: 
> 	 - /code/R_organised/1.1_pull-crossref-data.R # Gets data from crossref for each journal, dumps it out in a folder named and date-stamped
> 	 - /code/R_organised/1.2_join-clean-crossref-data.R #  # Cleans that data

Identifying the papers that relied on data, statistical analysis, and experiments was an iterative process. In each case we read target papers and devised a dictionary of terms meant to uniquely identify others like them. We extensively revised these dictionaries to arrive at terms that seemed to maximally discriminate for target reports. 

> Dictionary files: 
> 	- /output/experimental_dict.txt
>	- /output/quant_dict_w_cats.txt

The dictionaries were then used with custom functions to create document feature matrices, where each paper is an observation, each column a dictionary term, and each cell a count of that term.^[A custom function was preferable to existing text analysis libraries like `quanteda` because of our need to capture regular expressions and asterisks.] The DFM format made the papers amenable to large-scale analysis. In machine learning parlance, this process is known as feature engineering.

> Code files: 
> 	- /code/R_organised/3.1_classify-fulltext-papers.R	
> 
> Output files: 

For the first research question --- examining the presence of replication code and data in papers involving data analysis or statistical inference --- we hand-coded a total of 1,624 papers. 

> Hand-coded files: 
> 	- /output/handcoded_stat_papers.csv
> 
> Note that we hand-coded the files over a number of work sessions in Google Sheets and exported the combined result as a csv file.
> 
> Game theory dictionary: 
>	- /output/game_theory_dict.txt

For the second question --- examining what proportion of experiments were preregistered --- we hand-coded 518 papers with a single boolean category: whether the paper reported one or more original experiments. We defined this as any article containing an experiment where the researchers had control over treatment.

> Hand-coded files: 
> 	- /output/experiment_handcodes_round2.csv

We then trained two machine learning models --- the Support Vector Machine (SVM) and Naive Bayes (NB) binary classifiers --- to arrive at estimates for the total number of data analysis/statistical inference and experimental papers.^[As an additional robustness check to predict open data and statistical inference papers, we estimated a series of bivariate logistic regressions using the same DFMs. The predicted probability plots can be found in the appendix. These plots give a lower estimate than the machine learning models, though they are in the same broad range.] Note that in the final manuscript, we include only the SVM results and only the SVM results are now visible in the code. 

> Code files: 
>	- /code/R_organised/3.1_classify-fulltext-papers.R
> 

## Phase two: Identifying open data and preregistrations

We attempted to identify open data resources in seven ways. 

1. Using the Harvard Dataverse API, we downloaded all datasets held by all journals in our corpus who maintained their own, named dataverse (n=20);

2. We queried the Dataverse for the titles of each of the papers in our corpus and linked them to their most likely match with the aid of a custom fuzzy string matching algorithm. We validated these matches and manually established a string-similarity cut-off, setting aside the remainder;

3. We extracted from the full text of each paper in our corpus the link to its dataset on the Dataverse; note this had significant overlap with the results of the first and second queries);

For the above, the code files: 
>   -  /code/R_organised/4.1_query-dataverse-with-titles.R
>   -  /code/R_organised/4.2_pull-dataverse-links-from-papers.R
	
4. We downloaded the metadata listing the contents of these datasets, to confirm firstly that they had data in them, and secondly that it did not consist of only pdf or doc files. In cases where a list of metadata was not available via the Dataverse API, we scraped the html of the dataset entry and searched for text confirming the presence of data files;

5. We used regular expressions to extract from the full text papers references to "replication data," "replication materials," "supplementary files" and similar terms, then searched in the surrounding text for any corresponding URLs or mentions of author websites;

> Code files:
>    - /code/R_organised/4.4_precarious-data.R

We termed this 'precarious data' and have reported the results in the paper. 

The output files here are `./output/papers_w_precarious_data.csv` and, as part of the exploratory process of inspecting them we generated three excel files: 

- `./output/replication_links_author_website.xlsx`
- `./output/replication_links_author_website1.xlsx`
- `./output/replication_links_author_website2.xlsx`

6. We searched all of the full text papers for references to other repositories, including Figshare, Dryad, and Code Ocean, in the context of references to supplementary and replication materials. There were, however, only over a dozen meeting this criteria and we did not incorporate these into the results further.

This analysis is in `./code/R_disorganised/test_precarious_data.R`

7. As additional validation for DA-RT signatory journals specifically, we downloaded the html file corresponding to each article and/or the html file hosting supplemental material, then extracted all code and data-related file extensions to establish their open data status.

> Code files:
> 	- /code/R_organised/4.5_query-dart-journals.R
>	- /code/R_organised/4.3_query-jcr-jpr-data.R

We attempted to identify preregistration of experiments in the following ways:

1. We used regular expressions to extract from all of the experimental papers sentences that referred to "prereg" or "pre-reg", as well as any references to commonly used preregistration servers (osf, egap, and aspredicted), and then searched for the availability of the corresponding link to validate that the preregistration had taken place. Parts of this process --- for instance, searching author names in the Experiments in Governance and Politics (EGAP) registry to look for the corresponding paper --- involved time-consuming detective work;

> Code files:
> 	- /code/R_organised/5.2_identify-prereg.R
>	

2. We downloaded all EGAP preregistration metadata in JSON format from the Open Science Foundation Registry (https://osf.io/registries/discover), extracted from this file all osf.io links and unique EGAP registry IDs, and used command line utilities to search for them through the corpus of all the papers.

> Code files:
>	- /code/bash/rg_for_prereg_osf_egap_papers.txt 

We did not examine whether the published report conformed to the preregistration plan.

# Reproducibility

All packages were loaded with the `groundhog` library, so the `R` code should be reproducible. But it will be impossible to reproduce the full analysis without the ~6gb of text files of the papers (which are based on ~90gb of raw pdf and html). However, due to copyright concerns, we're not able to share these publicly. The rest of the analysis should be entirely reproducible.