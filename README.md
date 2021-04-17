
## Building and running via Docker

```
docker build -t chemotion-viz:latest .
```

```
docker run --rm -v $PWD/:/data --workdir="/data" chemotion-viz:latest R -e 'rmarkdown::render("Scripts/Workflow.Rmd", "html_document")'
```

### Jupiter Notebooks at binder

You can also use predefined Jupiter notebooks on binder to run the Workflow
interactively.

#### Vignette in Rstudio
[![Binder](https://mybinder.org/badge.svg)](https://mybinder.org/v2/gh/Flow191/Chemotion-Viz/main?urlpath=rstudio)

#### Notebook'ed vignette
[![Binder](https://mybinder.org/badge.svg)](https://mybinder.org/v2/gh/Flow191/Chemotion-Viz/main?filepath=Scripts%2FWorkflow.ipynb)


