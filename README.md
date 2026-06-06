# Prediktivní klávesnice

Toto je repozitář k mojí seminární práce Prediktivní klávesnice. Samotná seminárka je uložena v souboru Prediktivní klávesnice.pdf a veškerý popis kódu lze najít v ní.

Klávesnici si můžete vyzkoušet na https://jan-sliva.shinyapps.io/Predictive_keyboard_15MB/ (trvá asi 30 s, než se načte)

Práci jsem přihlásil do SOČ (https://www.soc.cz/). Soutěžní verzi práce a prezentaci lze nalézt na https://drive.google.com/drive/folders/1Bhi8l3XWLf4DnvZfH5t64FTs_aUTQTgD

---

# Predictive Keyboard

A statistical n-gram language model that predicts the next word(s) a user is likely to type, deployed as an interactive web application. The project was submitted to the Czech national student research competition [SOČ](https://www.soc.cz/). A full write-up (in Czech) is in `Prediktivní klávesnice.pdf`.

**Live demos** (allow ~30–60 s to load):
- [English — 15 MB corpus](https://jan-sliva.shinyapps.io/Predictive_keyboard_english_15MB/)
- [English — 15 MB corpus (older build)](https://jan-sliva.shinyapps.io/Predictive_keyboard_15MB/)
- Czech and Spanish variants are also deployed on shinyapps.io

---

## Theoretical Approach

### What kind of model is this?

This is **classical statistical NLP** — a **5-gram frequency language model**. There is no neural network, no deep learning, and no training loop. The model learns which words tend to follow other words by counting how often each word sequence appears in a large text corpus, then uses those counts to rank predictions.

### Step-by-step algorithm

1. **Corpus collection** — Raw text is sourced from Twitter, news, and blog data (EN-US) for English, plus equivalent corpora for Czech and Spanish. Subsets of 1.5 MB and 15 MB are used to study the effect of corpus size.

2. **Tokenization** — The `quanteda` package tokenizes text and strips punctuation, numbers, and URLs, producing a clean sequence of lowercase word tokens.

3. **Rare-word smoothing with a joker token** — Words below a frequency threshold are replaced with a special token `<>` (the "joker"). This collapses the long tail of rare words — typically ~80% of the vocabulary by type but only a small fraction by token count — into a single symbol. The substitution dramatically reduces sparsity in higher-order n-grams without discarding the contexts in which rare words appear.

4. **N-gram extraction** — `quanteda.textstats::textstat_collocations()` extracts all 2- through 5-grams above minimum count thresholds. Together with unigram counts, this produces five frequency tables.

5. **Conditional probabilities** — For each n-gram the conditional probability of the final word given its context is:

   ```
   P(word | context) = freq(context + word) / freq(context)
   ```

   These probabilities are stored alongside raw frequencies.

6. **Prediction at inference time** — Given the words already typed (up to the last four) and optionally a partial word being typed:
   - The n-gram tree is queried for all children of the matching context node.
   - Children are ranked by conditional frequency.
   - The **top 10** predictions are returned.
   - If the user is mid-word, a **trie prefix search** (`triebeard`) filters candidates to those beginning with the typed prefix.

7. **Back-off** — `GetBySeq` tries context lengths from 4 words down to 1 word and merges result sets, keeping the highest-probability prediction for each candidate word. This ensures useful predictions even when the exact long context was never seen in the corpus.

### Why n-grams and not a neural LM?

The project was scoped as a student research project that could run entirely in R, be fully interpretable, and be deployable as a small Shiny app without GPU infrastructure. Classical n-gram models are well-understood theoretically, fast at inference, and easy to serialize. The benchmarking section of the project (in `TimeTests/`) systematically evaluates multiple implementation strategies to squeeze maximum performance out of the R runtime.

---

## Implementation

### Technology stack

| Technology | Role |
|---|---|
| **R** | Primary language — all model logic, data processing, Shiny UI |
| **C++ via Rcpp** | Hot-path top-K selection for ranking child nodes |
| **Shiny** + **shinyjs** | Interactive web keyboard interface |
| **quanteda** + **quanteda.textstats** | Tokenization and n-gram / collocation counting |
| **triebeard** | Trie-based prefix matching for mid-word autocomplete |
| **readr**, **dplyr**, **stringr** | I/O, data wrangling, regex |
| **ggplot2**, **networkD3** | Visualization and graph exports |

### Core data structure: `NGramTree`

The model is stored as a custom **n-gram tree** defined in `NGramsTree/NGramTree.R` using R's S4 class system:

| Class | Description |
|---|---|
| `NGramRoot` | Root node; holds global settings (`maxResult = 10`, `joker = "<>"`) and first-level children |
| `NGramNode` | Internal or leaf node storing `name` (the word), `freq` (absolute count), `children`, `trie`, and `Highest` |
| `NGramBase` | Shared virtual class providing `children`, `trie`, and `Highest` slots |

Each node's **`children`** list maps words to child `NGramNode` objects, forming a prefix tree over word sequences. Each node also holds:
- a **`triebeard` trie** over its children's names for O(prefix-length) lookup of partial words, and
- a **`Highest`** integer vector of the indices of the top-K children by frequency, computed once at build time by C++ code in `NGramsTree/Sort.cpp`.

### Prediction functions

| Function | Trigger |
|---|---|
| `GetByNothing(root)` | Input box is empty — returns globally most-common words |
| `GetByPart(root, prefix)` | User has started typing the very first word |
| `GetBySeq(root, lastWords)` | At least one full word typed, predicting the next whole word |
| `GetBySeqAndPart(root, context, partial)` | Full context + partial next word — combines n-gram ranking with trie filtering |

### Performance: C++ top-K selection

`NGramsTree/Sort.cpp` (compiled via Rcpp) implements a partial-sort that extracts the top N child nodes by frequency without fully sorting the children list. This is critical because high-frequency root-level nodes can have tens of thousands of children. The `TimeTests/NGramTree/` directory contains nine measurement runs comparing R list, S3, S4, R5, and R6 implementations of the tree against this C++ approach.

### Pipeline: from raw text to deployed app

```
Raw corpus text
    │
    ▼  Text preprocessing/Extract.R
    │  Text preprocessing/RandomSelect.R
    │
Cleaned, subsampled tokens
    │
    ▼  Text processing/ForShiny2-withJokers.R
    │    - tokenize, apply joker substitution
    │    - count 1–5-grams
    │    - compute conditional probabilities
    │
N-gram frequency tables (1-gram.csv … 5-gram.csv)
    │
    ▼  NGramsTree/NGramTree.R  →  CreateNGramTree()
    │
In-memory NGramTree object
    │
    ▼  Shiny app/SaveMeta.R  →  CreateMeta()
    │    writes 1.csv–6.csv
    │
Serialized tree (CSV files)
    │
    ▼  Shiny app/*/load.R  →  LoadFromMeta()
    │
Running Shiny application
```

### Shiny application

Each app in `Shiny app/` is self-contained and works as follows:

1. `load.R` reads six CSV files and reconstructs the `NGramTree` in memory. File `1.csv` holds root metadata, `2.csv`–`5.csv` hold internal nodes with their frequency and child index ranges, and `6.csv` holds leaf nodes.
2. `app.R` renders a text area and up to ten prediction buttons. On every keystroke:
   - The current sentence (text after the last `.`, `?`, `!`, or `"`) is extracted.
   - If the last character is a space, `GetBySeq` is called with the last up to four words.
   - Otherwise `GetBySeqAndPart` (or `GetByPart` if no prior full word) is called.
3. Clicking a prediction button appends the predicted word and a space.

### App variants

| Folder | Language | Corpus |
|---|---|---|
| `Predictive_keyboard_1.5MB` | English | 1.5 MB subsample |
| `Predictive_keyboard_english_15MB` | English | 15 MB subsample |
| `Predictive_keyboard_czech_15MB` | Czech | 15 MB subsample |
| `Predictive_keyboard_spanish_15MB` | Spanish | 15 MB subsample |

---

## Repository Structure

```
Predictive-keyboard/
├── Prediktivní klávesnice.pdf     # Full thesis (Czech)
├── Predictive-keyboard.Rproj      # RStudio project
│
├── Data/                          # Intermediate n-gram CSV tables
│   └── 1-gram.csv … 5-gram.csv
│
├── NGramsTree/                    # Core model implementation
│   ├── NGramTree.R                # S4 tree classes + build/query functions
│   ├── Sort.cpp                   # Rcpp top-K selection
│   └── test.R
│
├── Text preprocessing/            # Corpus preparation scripts
│   ├── Extract.R
│   └── RandomSelect.R
│
├── Text processing/               # N-gram extraction pipeline
│   ├── ForShiny2-withJokers.R     # Main pipeline (with joker smoothing)
│   ├── ForShiny2.R                # Pipeline without jokers
│   └── …
│
├── Shiny app/                     # Deployable web app variants
│   ├── Predictive_keyboard_english_15MB/
│   ├── Predictive_keyboard_czech_15MB/
│   ├── Predictive_keyboard_spanish_15MB/
│   ├── Predictive_keyboard_1.5MB/
│   └── SaveMeta.R, Resave.R, …
│
├── Graphs/                        # Visualization scripts and outputs
├── TimeTests/                     # Performance benchmarks (Rmd + R)
└── old/                           # Early prototypes (data.table, hash)
```

---

## Running Locally

### Prerequisites

Open `Predictive-keyboard.Rproj` in RStudio and install the required packages:

```r
install.packages(c(
  "shiny", "shinyjs", "stringr", "readr", "dplyr",
  "quanteda", "quanteda.textstats",
  "triebeard", "Rcpp",
  "ggplot2", "data.table", "hash",
  "rbenchmark", "purrr", "R6"
))
```

### Run an existing app (no corpus needed)

Each app folder already contains pre-serialized CSV data, so no corpus is required:

```r
shiny::runApp("Shiny app/Predictive_keyboard_english_15MB/app.R")
```

### Rebuild the model from a corpus

1. Point the path variables in `Text preprocessing/Extract.R` and `Text processing/ForShiny2-withJokers.R` to your corpus files.
2. Run `ForShiny2-withJokers.R` — this produces the n-gram frequency tables and builds the tree object `data_tree`.
3. Serialize the tree:

```r
source("Shiny app/SaveMeta.R")   # writes 1.csv–6.csv into the target app folder
```

### Run benchmarks

```r
source("TimeTests/NGramTree/Test.R")
rmarkdown::render("TimeTests/Maximum/Maximum.Rmd")
```
