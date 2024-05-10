This folder contains the code and data files we used and created to build a provision LLM-powered classification pipeline for the social science papers in our corpus. 

Essentially the code finds all the txt files (converted pdf/html files of journal papers) in a directory, gets the dois, matches them to our original hand-coded dataset, and tries some classification operations using the claude-haiku model. We wanted to assess whether a relatively cheap commercial model could perform well enough at our classification task.

In the end, the accuracy was insufficient for our use case. We suspect that fine-tuning a different commercial model, or an open source alternative, would ultimate work better than the model we did use (a support vector machine), but we exhausted our time and resource constraints to pursue that path. 

A more detailed explanation of why we did this and what we found is in the letter to the editors and reviewers as part of the revised manuscript.

Matthew Robertson and Bermond Scoggins, 2024-05-11
