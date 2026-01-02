ICC Shiny App — Managing Intra-Cluster Correlation Coefficients (ICCs) in Cluster Randomised Trials

The ICC Shiny App is an open-source R Shiny application that helps researchers, statisticians, and students estimate, interpret, select, and incorporate uncertainty around ICC values when designing Cluster Randomised Trials (CRTs). It integrates multiple published estimation methods, supports uploading user data, and includes a curated ICC database to encourage transparency and improved reporting practice in CRT design.

👉 Live App: https://olivierquintin.shinyapps.io/The_ICC_Shiny_app/

👉 Source Code: https://github.com/oquintin/The_ICC_Shiny_App

✨ Key Features

Choose ICC values for sample size calculation

Derive distributions from multiple prior estimates

Apply Turner et al. prior models

Select conservative or pragmatic values

Account for uncertainty

Incorporate uncertainty around a single ICC estimate

Multiple approaches (e.g., Fisher transformation, Swiger & Searle)

Estimate ICCs from your data

Supports continuous, binary, count, time-to-event, ordinal outcomes

Multiple estimators (mixed models, GEE, ANOVA, Pearson, resampling, etc.)

Compare estimates across approaches

ICC Database

Search curated ICC estimates

Upload new ICC estimates to support community transparency

Education support

Built-in tutorials

Concept explanations

Visualisations for interpretation

🎯 Intended Audience

This tool is designed for:

CRT trial statisticians and methodologists

Early-career statisticians

Clinical and public-health researchers

PhD students and trainees learning CRT methodology

Educators teaching CRT design

It is not intended to replace expert statistical input, but to:

support transparent decision-making,

improve ICC understanding,

facilitate better adherence to CONSORT recommendations,

and provide an accessible platform for methodological best practice.

📦 Installation (Local Use)

You can run the app locally if preferred.

Requirements

R ≥ 4.0

R packages listed in DESCRIPTION

Run locally
library(shiny)
runGitHub("oquintin/The_ICC_Shiny_App")

🚀 How to Use

The app contains five main tabs:

1️⃣ Welcome

Overview

Tutorial videos

2️⃣ Previous Estimates

Upload ICC estimates from literature

Construct posterior distributions

Select design ICC

3️⃣ Uncertainty

Model uncertainty around a single ICC

4️⃣ Data

Upload datasets and estimate ICCs

5️⃣ Database

Search ICC repository

Contribute estimates

📊 Example Use Cases

Selecting an ICC for CRT sample size planning

Comparing alternative ICC estimation methods

Teaching ICC concepts to students

Supporting grant/pilot justification

Enhancing transparency in protocol development

🧑‍⚖️ License

This project is released under the MIT License.
See LICENSE for details.

🔄 Versioning & Maintenance

This project follows semantic versioning:

MAJOR – substantial functionality change

MINOR – feature additions and enhancements

PATCH – bug fixes / minor updates

Latest stable version: v1.0.0

For maintenance and development commitments, see GOVERNANCE.md.

🧪 Validation & Reliability

Uses published and peer-reviewed ICC estimation methods

References are documented in the manuscript and app

Methods correspond to those recommended in CRT methodological literature

We welcome independent validation and replication work.

🤝 Contributing

We welcome:

bug reports

feature suggestions

database contributions

code contributions

methodological collaboration

Please see CONTRIBUTING.md.

📬 Contact

Lead developer:
Olivier Quintin
Queen Mary University of London
📧 olivierquintinpro@gmail.com

We encourage collaboration and community involvement.

📝 Citation

If you use this app, please cite:

Quintin O., Sarkodie S.K., Hamborg T.
ICC Shiny App: Managing intra-cluster correlation in cluster randomised trials.
