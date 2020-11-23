# Editorial Highlights

A collection of highlighted publications in [Nature](https://www.nature.com) and [Science Magazine](https://science.sciencemag.org).


## Data

12'376 highlighted publications: 6'714 under *Research Highlights* in *Nature* and 5'662 under *Editor's Choices* in *Science Magazine* between 2005 and 2020.

You can download the lastest update as [CSV](https://raw.githubusercontent.com/zambujo/editorials/main/data/research_highlights.csv).


### Description

|Variable |Class     |Description                       |
|:--------|:---------|:---------------------------------|
|journal  |string    |`science`, `nature`               |
|hl_year  |numeric   |`2005` - `2020`                   |
|hl_topic |string    |research topic (when available)   |
|hl_title |string    |title of the highlight            |
|hl_url   |string    |URL of the highlight              |
|citation |string    |extracted citation                |
|resource |string    |DOI (when available)              |


## DIY

1. Rename or remove the CSV files in `data-raw/`
2. Run `data-raw/harvest_all.R` (takes a few hours)
3. Run `data-raw/prepare_data.R`

## License

Distributed under the MIT License.

## Related Work

[![In Search of Outstanding Research Advances](https://zenodo.org/badge/DOI/10.5281/zenodo.4155204.svg)](https://doi.org/10.5281/zenodo.4155204)  
[![An open dataset of scholarly publications highlighted by journal editors](https://zenodo.org/badge/DOI/10.5281/zenodo.4275660.svg)](https://doi.org/10.5281/zenodo.4275660)  
[![Prototyping the creation of an open dataset of editorial highlights](https://img.shields.io/badge/arXiv-2011.07910-b31b1b.svg)](https://arxiv.org/abs/2011.07910)  
