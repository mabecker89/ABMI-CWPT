## Clean Water Prioritization Tool (CWPT) 
### Produced by the Alberta Biodiversity Monitoring Institute (ABMI)
#### Project Developers: Marcus Becker, Thomas Habib

---

The Clean Water Prioritization Tool (CWPT) has been developed to provide guidance to interested stakeholders in maintaining and enhancing water-related ecosystem services throughout their area of interest. The CWPT was built to provide practical guidance on where to target conservations and management efforts to achieve ecosystem service gains in an efficient manner. 

The CWPT is a mapping product designed to provide users information at the quarter-section scale. Landowners, decision-makers, and extension agents can use the CWPT to determine the relative importance of each quarter-section in contributing to the provision of clean surface water, as well as receive guidance on management actions appropriate to that particular quarter. 



Features of the CWPT include:



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

