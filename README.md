# wood-density-Cirad

The `wood-density-Cirad` GitHub repository includes the R script, data and results associated to the following scientific article:

**Vieilledent G., F. J. Fischer, J. Chave, D. Guibal, P. Langbour and J. Gérard.** New formula and conversion factor to compute tree species basic wood density from a global wood technology database. _bioRxiv_. 274068. \[DOI: [10.1101/274068](https://doi.org/10.1101/274068)\].

## Summary

Basic wood density is an important ecological trait for the study of woody plants. It is important to characterize species performance and fitness in community ecology, and to compute tree and forest biomass in carbon cycle studies. While wood density has been historically measured at 12% moisture for construction purpose, it is convenient to convert this measure to basic wood density, i.e. the ratio of dry mass over green volume. Basic wood density can then be used to compute tree dry biomass from living tree volume.

Here, we show that previous conversion factors used to convert densities at 12% moisture into basic wood densities are inconsistent. We derive a new, exact formula to compute the basic wood density _Db_ from the density at moisture content _w_ denoted _Dw_, the fibre saturation point _S_, and the volumetric shrinkage coefficient _R_. We estimated a new conversion factor using a global wood technology database where values to use this formula are available for 4022 trees collected in 63 countries (mostly tropical) and representing 872 species.

Based on theory and data, we found that basic wood density could be inferred from the density at 12% moisture using the following formula: _Db = 0.828 D12_. This value of 0.828 provides basic wood density estimates 4-5% smaller than values inferred from previous methods.

This new conversion factor should be used to derive basic wood densities in global wood density databases. This would prevent overestimating global forest carbon stocks and allow predicting better tree species community dynamics from wood density.

## Cirad wood density database (CWDD)

[![Cirad](img/Logo-Cirad.png)](<http://www.cirad.fr>)

The Cirad (<http://www.cirad.fr>) is the French agricultural research and international cooperation organization working for the sustainable development of tropical and Mediterranean regions. Since the 1950s, scientists at Cirad have compiled data on wood characteristics for hundreds of tree species around the world. Data have been compiled in a unique global wood technology database.

Combining our research results with the data from the wood technology database, we were able to compute the basic wood density for each tree and derive the Cirad wood density database (CWDD). The CWDD is available as a comma-separated values file: `Cirad-wood-density-database.csv`. It includes basic wood-density data _Db_ (in g/cm3) for 4022 trees collected in 63 countries (mostly tropical) and representing 872 species. Basic wood-density has been computed from the wood-density at 12% moisture _D12_ (in g/cm3), the volumetric shrinkage coefficient _R_ (in %/%), and the fibre saturation point _S_ (in %) for 3832 trees. When _R_ or _S_ values were not available (for 190 trees), the basic wood-density was estimated from _D12_ using a conversion factor of 0.828: _Db = 0.828 D12_.

The development version of the data-base can be downloaded [here](https://github.com/ghislainv/wood-density-Cirad/blob/master/Cirad-wood-density-database.csv).

## Versioning and archiving on the Cirad Dataverse

Versions of this repository, including versions of the Cirad wood density database, are archived on the Cirad Dataverse repository: 

**Vieilledent G., F. J. Fischer, J. Chave, D. Guibal, P. Langbour and J. Gérard.** Code and data for: New formula and conversion factor to compute tree species basic wood density from a global wood technology database. _CIRAD Dataverse_. \[DOI: [10.18167/DVN1/KRVF0E](https://doi.org/10.18167/DVN1/KRVF0E)\].
