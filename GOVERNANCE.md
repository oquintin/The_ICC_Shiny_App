Governance and Sustainability Plan for the ICC Shiny App
1. Project Vision

The ICC Shiny App was developed to support better design, analysis, and reporting of Cluster Randomised Trials (CRTs) by providing accessible tools for:

estimation of intra-cluster correlation coefficients (ICCs),

appropriate selection of ICC values for trial planning,

incorporation of uncertainty around ICC estimates,

and promotion of transparency through structured data sharing.

The project aims to translate methodological developments from the CRT literature into practical tools for researchers, statisticians, educators, and trial methodologists.

2. Project Ownership and Leadership

The ICC Shiny App is led and maintained by:

Olivier Quintin
Wolfson Institute of Population Health
Queen Mary University of London (QMUL), UK

Scientific input and collaboration are provided by co-authors and contributors acknowledged in the repository and associated publications.

3. Roles and Responsibilities
Lead Maintainer

The lead maintainer is responsible for:

Reviewing and addressing issues raised on GitHub

Reviewing and approving pull requests

Ensuring scientific and methodological correctness of implemented methods

Maintaining app stability and functionality

Overseeing documentation updates

Overseeing governance of the ICC database

Contributors

Contributors may:

Propose improvements or new features

Report bugs

Contribute code

Suggest methodological extensions

Contribute ICC estimates to the database

All contributions are reviewed by the maintainer before integration.

4. Versioning and Release Policy

The ICC Shiny App follows semantic versioning:

MAJOR version: substantial changes in functionality or structure

MINOR version: feature additions and methodological extensions

PATCH version: bug fixes and minor improvements

A version history is maintained in the repository.

5. Update and Maintenance Policy

The project is actively maintained with commitment to:

Ongoing bug fixes as issues are identified

Incorporation of new ICC estimation methods as they appear in the methodological literature

Periodic review of documentation and tutorials

Continuous monitoring of app functionality

Major methodological additions are based on peer-reviewed developments.

6. Governance of the ICC Database (REDCap Infrastructure)

The ICC database is a central component of the app and is implemented using REDCap, hosted by the Pragmatic Clinical Trials Unit (PCTU) at Queen Mary University of London. This infrastructure provides:

Secure data storage

Structured data entry forms

Professional data management support

Long-term institutional hosting and sustainability

Database Structure and Data Entry

Users can contribute ICC estimates directly through a structured REDCap form embedded within the app. Completion of several key fields is mandatory for submission, including:

ICC value

Trial population description

Type of cluster

Outcome description

Additional fields allow contributors to provide contextual information, including a publication link, DOI, or PMID where applicable. Entries containing such references may be considered more easily verifiable by users of the database.

Curation and Data Use

Submissions are not reviewed prior to inclusion in the database. The database is intended as an open, community-contributed resource to promote transparency and sharing of ICC estimates across studies and settings.

Data cleaning and validation procedures are undertaken when the database is used for secondary analyses, such as methodological investigations or comparisons of ICC estimates across estimation methods or contexts. These processes are conducted as needed depending on the analytical objective.

Entries may be edited or removed if found to be incorrect, unclear, or misleading.

Contributors remain responsible for the accuracy and ethical appropriateness of submitted data.

7. Longevity and Sustainability

To ensure long-term accessibility:

The source code is fully open-source and archived on GitHub

The app can be run locally independently of the hosted version

The REDCap database is supported by institutional infrastructure at QMUL

Other researchers may fork and continue development if needed

The combination of open-source code and institutional data hosting ensures that the project does not depend solely on a single individual.

8. Community Involvement

The project encourages:

Feedback from users

Educational use in CRT teaching

Collaboration on methodological extensions

Contributions to the ICC database

Decisions regarding development are made transparently and documented through GitHub issues and commits.

9. Code of Conduct

All contributors and users are expected to follow respectful, inclusive, and professional behaviour. See CODE_OF_CONDUCT.md.

10. Contact

For governance or collaboration queries:

Olivier Quintin
Queen Mary University of London
olivierquintinpro@gmail.com
