---
title: "The ICC Shiny App: an open-source platform for estimating, selecting, reporting,
  and applying intra-cluster correlation coefficients in cluster-randomised trials"
authors:
- name: Olivier Quintin
orcid: 0000-0002-9354-8897
  affiliation: 1
- name: Samuel K. Sarkodie
orcid: 0000-0002-9296-8216
  affiliation: 2
- name: Thomas Hamborg
orcid: 0000-0001-7154-3983
  affiliation: 1
date: 6 February 2026
output:
  word_document: default
  html_document:
    df_print: paged
bibliography: paper.bib
affiliations:
- name: Wolfson Institute of Population Health, Queen Mary University of London, UK
  index: 1
- name: Department of Bioengineering and Therapeutic Sciences, University of California,
    San Francisco, USA
  index: 2
---

# Summary

Cluster-randomised trials (CRTs), in which groups of individuals such as
hospitals, schools, or communities are randomised rather than
individuals themselves, are widely used in clinical and public health
research [@Murphy2006]. A key parameter in both the design and analysis of CRTs is the
intra-cluster correlation coefficient (ICC), which quantifies the
similarity of outcomes among individuals within the same cluster [@Eldridge2009] 
[@Killip2004] [@Liljequist2019]. Even small misspecifications of the ICC can have a substantial impact on the
required sample size [@Teare2014] through the design effect [@Hemming2016], potentially leading to
trials that are too small to answer the research question, or much
larger than necessary.

Accordingly, the CONSORT extension for CRTs [@Campbell2012] recommends reporting the ICC
value used in sample size calculation and ICC estimates from statistical
analyses and their uncertainty in trial publications. However, a review
in 2011 evaluated the impact of these recommendations [@Ivers2011] and highlighted
that only 61% of trials that should have accounted for clustering in the
design for those presenting sample size calculation did so, and only 16%
reported ICC values. A more recent review in 2022 indicates that
reporting has not notably improved; in fact, a higher proportion of
trials still fail to report estimated ICCs after publication of the
CONSORT 2010 statement compared with before (44% vs. 31%) [@Offorha2022]. Furthermore,
ICC estimation can be challenging, particularly for non-continuous
outcomes or complex designs. Finally, uncertainty around ICC values is
rarely incorporated into sample size calculations [@Sarkodie2023], even though this
uncertainty can meaningfully affect power [@Sarkodie2023] [@Sarkodie2024]. These challenges contribute
to inefficient trial design and limit the opportunity for reuse of
existing evidence.

We present the ICC Shiny App, an open-source R Shiny application [@Chang2012]
developed to support the estimation, interpretation, selection,
reporting, and appropriate use of ICCs in CRT planning and analysis. The
app integrates multiple published ICC estimation methods across a wide
range of outcome types, allows users to formally incorporate uncertainty
around both single and multiple ICC estimates, and provides access to a
curated database of previously reported ICC values. The application is
freely accessible online and can also be run locally, supporting both
applied research and education.

# Statement of Need

Although the methodological role of ICCs in CRTs is well established,
several persistent challenges affect their practical implementation.
First, ICC estimates are frequently unavailable, poorly reported, or
insufficiently described in published studies [@Ivers2011] [@Offorha2022]. When ICCs are reported,
information is often incomplete, limiting their usefulness for future
trial planning. Second, estimating ICCs can be technically demanding,
particularly for binary, count, time-to-event, or ordinal outcomes, for
which multiple competing estimators exist and may yield different
values [@Nakagawa2017] [@Wu2012] [@Westgate2019].

Third, even when relevant ICC estimates are available, uncertainty
around these estimates is rarely propagated into sample size
calculations [@Sarkodie2023]. Instead, a single fixed ICC value is typically selected,
increasing the risk of trial misspecification if the true ICC differs
meaningfully from the chosen value. Finally, while guidance exists in
the methodological literature on how to handle ICC uncertainty and how
to combine information from multiple previous studies, these approaches
are not easily accessible to applied researchers because of their
complexity involving the Bayesian framework.

Existing software tools for CRT design primarily focus on sample size
calculation [@Hemming2020]. Few tools provide support for selecting appropriate ICC
values for a wide range of different outcome types, comparing
alternative estimation methods, or formally accounting for uncertainty.
Practical, accessible software to support these aspects of ICC
management is therefore limited.

The ICC Shiny App addresses these gaps by providing a unified platform
that enables users to:

1. estimate ICCs from user-supplied datasets for a wide range of
outcome types using multiple estimation approaches;
2. derive ICC values from previous studies and construct prior
distributions using an established method [@Turner2005] for combining multiple
estimates;
3. incorporate uncertainty around single ICC estimates using
published statistical methods [@Lewis2021]; and
4. access and contribute to a structured, searchable ICC database.

The app is intended to support statisticians, methodologists, applied
researchers involved in CRT design, and educators. It is not designed to
replace specialist statistical expertise, but rather to facilitate
transparent, evidence-based decision-making, improve understanding of
ICC-related concepts, and promote better adherence to reporting
recommendations such as the CONSORT extension for CRTs [@Campbell2012].

# Functionality and Implementation

The ICC Shiny App is implemented in R using the Shiny web application
framework [@Chang2012] and relies on established, peer-reviewed R packages [@Chakraborty2018] [@LCarrasco2022] wherever
possible. The application is organised into five main components, each
addressing a specific aspect of ICC management.

The "Previous estimates" component allows users to upload ICC estimates
extracted from the literature or other sources. Users can then construct
posterior distributions using several modelling strategies based on
published frameworks for combining multiple ICC estimates [@Turner2005], including
approaches that account for exchangeability between studies and outcomes
and allow for multiplicative or additive weighting. These features
enable users to reflect the relevance of individual studies or outcomes
to a target setting. An ICC value may then be selected from the
resulting distribution, for example by choosing a high percentile to
obtain a conservative design value.

The "Uncertainty" component supports the incorporation of uncertainty
around a single ICC estimate. Users specify the ICC value, the number of
clusters, and the number of individuals, and the app generates ICC
distributions using methods such as Fisher's transformation and
approaches described in the methodological literature [@Lewis2021]. Summary
statistics and graphical outputs are provided to support informed
selection of appropriate values and to visualise the potential impact of
uncertainty.

The "Data" component enables users to estimate ICCs from uploaded
datasets. Continuous, binary, count, time-to-event, and ordinal outcomes
are supported. Multiple estimation methods are implemented, including
mixed-effects models [@Donner1986], generalised estimating equation-based estimators [@Liang1986],
ANOVA-type estimators [@Atenafu2012], Pearson estimators [@Liu2016], and resampling-based
approaches [@Chakraborty2016]. Displaying results from several estimators within a single
interface allows users to compare estimates and better understand the
sensitivity of the ICC to modelling assumptions.

The "Database" component provides access to a curated repository of ICC
estimates. The ICC database has been developed using REDCap and is
hosted by the Pragmatic Clinical Trials Unit (PCTU) at Queen Mary
University of London, in collaboration with the PCTU's professional data
management team. This infrastructure ensures secure storage, structured
data entry, and long-term sustainability. Users can search existing ICC
entries using structured metadata fields and contribute new estimates to
support transparency and evidence reuse.

Finally, the app includes educational material, including tutorial
videos, explanatory text, worked examples, and graphical visualisations.
These features support both self-directed learning and use in formal
teaching settings.

# Comparison with Existing Tools

Several software tools are available to support sample size calculation
for CRTs [@Hemming2020] [@Ouyang2022]. However, most require users to input fixed ICC values and
provide limited guidance on how that value should be chosen. Tools that
focus on ICC estimation typically implement a single estimator or are
restricted to specific outcome types [@Ouyang2023], and rarely address uncertainty.
Resources such as the *Clustered Outcome Dataset Bank* provide valuable
collections of correlation parameters derived from clustered data and
represent an important contribution to improving transparency and reuse
of design parameters in CRTs [@Korevaar2021]. However, such resources primarily function
as data repositories and do not allow user implementation.

To our knowledge, no existing tool integrates multiple ICC estimation
approaches across outcome types, formal methods for incorporating
uncertainty from both single and multiple estimates, a user-contributed
ICC database supported by professional data infrastructure, and an
educational interface within a single platform. The ICC Shiny App
therefore fills an important methodological and practical gap in the CRT
design ecosystem.

# Availability

The ICC Shiny App is freely available online and may also be run
locally.

Live application:
<https://olivierquintin.shinyapps.io/The_ICC_Shiny_app/>

Source code:
<https://github.com/oquintin/The_ICC_Shiny_App>

# Software Sustainability

The ICC Shiny App is released under an open-source license and is
continually developed following established software sustainability
practices. The source code is publicly available, versioned using
semantic versioning, and accompanied by documentation, contribution
guidelines, a code of conduct, and a governance plan describing
maintenance responsibilities and database curation procedures.

The ICC database benefits from institutional support through the PCTU at
Queen Mary University of London, which hosts the REDCap infrastructure
and provides professional data management expertise. Community
contributions are encouraged to support ongoing development,
methodological extension, and long-term sustainability.

# Acknowledgements

We thank colleagues at the Pragmatic Clinical Trials Unit at Queen Mary
University of London for their support in developing and hosting the ICC
database, as well as users who have provided feedback on the development
and functionality of the ICC Shiny App.

# References