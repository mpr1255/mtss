This codebook identifies all of the variables in the primary dataset for our paper. 

`doi`
The digital object identifier of the paper, as identified in Crossref

`od_doi`
The DOI -- or URL in some instances -- of the open data (OD) for the paper

`ir_bool`
Boolean value if an international relations journal as identified by Clarivate's Journal Citation Report

`ps_bool`
Boolean value if a political science journal as identified by Clarivate's Journal Citation Report

`abbrev_name`
Abbreviated name of the journal

`prereg_score`
The preregistration status of the paper. 0 = ; 1 = ; 2 = .

`exp_bool`
Boolean value if the paper is identified as experimental by our Support Vector Machine classifier.

`data_bool`
Boolean value if the paper is identified as data analysis by our Support Vector Machine classifier

`stat_bool`
Boolean value if the paper is identified as statistical inference by our Support Vector Machine classifier

`i.od_doi`
The second DOI -- or URL in some instances -- of the open data (OD) for the paper

`journal_name`
Name of the journal as identified in Crossref

`i.abbrev_name`
The second abbreviated name of the journal

`created`
Creation date, per Crossref field

`deposited`
Deposit date, per Crossref field

`published_online`
Date the paper was published online, per Crossref field

`published_print`
Date the paper was published in print, per Crossref field. Note that we primarily filtered on this field, given the lack of correspondence between `published_online` and the usual time the paper first appeared.

`indexed`
Date the paper was index, per Crossref field.

`issn`
International Standard Serial Number of the journal, typically in an untidy format

`pissn`
The (print) International Standard Serial Number of the journal, clean

`eissn`
The (electronic) International Standard Serial Number of the journal, clean

`publisher`
Publisher of the paper as identified by Crossref

`title`
Title of the paper as identified by Crossref

`od_bool`
Boolean value, derived from whether there is text in the `od_doi` or `i.od_doi` fields

`dart_year`
Year the journal signed onto the Data Access and Research Transparenty (DA-RT) statement

`dart_bool`
Boolean value for whether the journal signed DA-RT

`published_print_ym`
Year and month of publication, derived from the `published_print` variable using basic string manipulation

`published_print_ym_date_format`
Year and month of publication as POSIXct date-time object, derived from the `published_print` variable using the `lubridate` package

`published_print_year`
Year of publication (string), derived from `published_print` variable

`published_print_month`
Month of publication (string), derived from `published_print` variable

`pd_bool`
Boolean value for 'precarious data', derived from whether or not `pd_context` had text or was empty

`pd_context`
A string extracted from the full text of the papers discussing the location of data that is neither on the Harvard Dataverse or a journal's own, clearly advertised data repository hosted elsewhere (i.e., 'precarious')