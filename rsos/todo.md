
Here's the structure of this document and imo our general game plan.
1. The repsonse to reviews
2. Bermond's todo list
3. Matthew's todo list

Bermond is going to do 1 as well as his own tasks; so that will be basically summarizing what he did as well as what matthew did.

So most 'tasks' appear twice here. Once in the response to reviews, once in our todo lists.

# Response to reviews (BERMOND TO WRITE)

This should be with numbered points in a markdown doc. I'll add my pieces and Bermond knits and sends etc.

I think the 'points' are going to be like this: 

POINTS FROM EDITOR:

* Policy prescriptions

* Empirical claims in introduction supported with citations
- You go ahead and do this and explain having done so. 
 
* Headings 2, 3, and 4 as subheadings of intro

* Explicit definition of 'replication'

* Upload the finalised Github repo to Zenodo or OSF or Dataverse and provide DOI

POINTS FROM REVIEWER 1:

* Timing of start of open science movement

* Issues to do with preregistration
- THOUGHTS: Bermond, we might want to significantly circumscribe our claims about prereg. We do NOT have the bandwidth(/it is not a priority for us) to look at every article related to prereg and classify them as either having needed prereg or not. There may be some very straightforward criteria we could use, but basically I think we need to be clear that this is just a small extension on the main findings and this area needs significantly more work -- for example to identify WHICH studies _should_ have been preregistered as well as the alignment of the preregs that do exist with the published papers. We could well have looked into that question but didn't due to scope/time/issues. 

* On reporting journals
- THOUGHTS: We definitely still think there is value in actually _CHECKING_ whether journals that have a policy of open replication data as a condition of publication actually follow that policy. Our study was triggered by wanting to see whether the DART commitment was being upheld. So just because journals say they will do something it does not mean they will always do it. 

* Description of our procedures
- We added a lot more (another page) of description and put footnotes to the code files where that processing logic happens

* Many Labs studies on replications
- THOUGHT: If it's not essential for our argument, just cut this sentence. We don't need a new fight. 

* First 11 pages
- THOUGHT: Make some cuts, see if you can cut a page or a page and a half. 
 
* Improve classifier model
- Changed out ML to gpt-3.5-turbo with a dictionary filter first and chunking etc. 

* Validation of classifier and CIs based on the result
- Our method for validating the LLM is described in our appendix. Basically we should random sample 100 of the classifications -- including the ones we filtered out (i.e. they have to have certain keywords to even hit the model) -- and then mark them ourselves. And then we have an accuracy score.
- We then also adjusted our estimates with some CIs based on XYZ.

POINTS FROM REVIEWER 2: 

* Condensing the review
- Per comment from R1 also, we have reduced this by 1.5 pages. (Bermond, you'll recall the guy from the fake uni also said this.)

* Description of our procedures
- We have added an additional page fo description of our code, with footnotes to the actual code files, that describe the logical procedures we followed to gather, link, and preserve the data.
- BERMOND: What is this?? " What procedure did the authors follow to revise dictionaries and evaluate discrimination?"

* Section 5.2 on linking papers to data
- We agree that this is a crucial step that influences the validity of our results
- We have added several paragraphs explaining both our procedures and how we tested that they were valid.

* Properly identifying preregistration studies

* Why we split IR and political science

* More detail in results
- We plot time series in more journals
- How many have no replication data? 
- How many above 50% How many above 90%
- More

* Policy suggestions
- The only thing I think we can talk about here is just to suggest that Crossref add this as a metadata field, and when journals expose their data to crossref they include this as a field. If this was done, it would then be immediately clear _HOW_ to even measure the existence of such data.
- The absence of data could go with a note too -- so there could be a few pieces of metadata, and in any case it would be clear one way or the other whethere (1) there was data, (2) there was a reason for not being data. This would not guarantee that the presence of data guarantees replicability, but it would be _something._

# Bermond TO DO LIST
NOTE: 

* Correct erroneous classification of JCR 2020 PS and IR journals (quite a few IR journals are also on the PS journals -- the way they were merged is that PS overrode IR so that IO/RIO are classified as a PS journal).

* Policy prescriptions

* Empirical claims in introduction supported with citations
 
* Headings 2, 3, and 4 as subheadings of intro

* Explicit definition of 'replication'

* Upload the finalised Github repo to Zenodo or OSF or Dataverse and provide DOI

* Timing of start of open science movement
* Issues to do with preregistration
* On reporting journals

* Validation of classifier and CIs based on the result

* Condensing the review

* Why we split IR and political science
* More detail in results

# Matthew TO DO LIST

* Description of our procedures
* Improve classifier model
* Section 5.2 on linking papers to data


# Both todo:
* Properly identifying preregistration studies 

- We need to discuss this -- both identifying them but also the issue of what we are saying about them anyway. R1 was not too pleased with how we seemed to say any experiment should be prereg?? May need to tighten those claims. You deal with the wordcel side but I can deal with the better method of identifying them. And maybe like the quant paper classification we can use a dictionary filter first and then run it into GPT3.5 and get a result on both whether or not it's an experiment or it mentions prereg or if there's a simple criteria then we may even be able to know if that experiment SHOULD HAVE been preregged.

	- Prereg not incompatible with exploration -- despite insufferable academics.
	- Every experiment should be preregged -- detail their measures etc -- and not get in the way of exploration. 
	- Still good to know even if prereg is not normatively necessarily. 
	- Pre-analysis (PAP) included in dictionary. 
	