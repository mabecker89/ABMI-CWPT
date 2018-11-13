# Clean Water Prioritization Tool

As part of the Ecosystem Services Assessment (ESA), the Alberta Biodiversity Monitoring Institute has developed a Clean Water Prioritization Tool (CWPT) to help guide municipalities in maintaining and enhancing water-related ecosystem services throughout their jurisdiction. Improvement of water quality is an important goal among many municipalities, and practical guidance on where to target conservation and management efforts to achieve ecosystem service gains in an efficient manner is needed. 

The CWPT is a mapping product designed to give landowners, decision-makers, and extension agents information on the relative importance of each quarter-section in contributing to the provision of clean surface water, as well as guidance on appropriate management actions.

Ideas to improve useability of the tool interface:

* Add a search bar to look up specific QS's;
* Improve filtering in that second map
  + Custom filter? User can set a range of the landscape importance score. Might have to look into Shiny.
* Add plot of QS p-load, p accumulation, and downslope retention to document so user can see distribution
* Customize binning (i.e., not just 10 equal, but natural breaks, weighted, jenks, etc)
* Customize averaging of index scores
* Display raw phosphorus values for each QS? 
* Add an explainer / relevant literature related to available management practices 

Improve the back-end modelling:

* Run using catchment data from Jerome, and/or smaller cell size DEM (15m?) 

To-do in the longer term:

* Write technical documentation
* Evaluate at watershed level? Not just county. 

