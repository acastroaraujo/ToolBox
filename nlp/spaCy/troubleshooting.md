

## Troubleshooting


If you lack admin priviledges for some reason, the language models might not install. 

See here: https://spacy.io/usage/#troubleshooting

The solution that worked for me: 

1. Download the tar file that corresponds to the language model directly from github.

2. Type this in terminal: `python -m spacy link /Users/andres.casstroaraujo/en_core_web_sm-2.2.0.tar.gz custom_link_to_model`

3. Maybe it was: `pip install /Users/you//Users/andres.casstroaraujo/en_core_web_sm-2.2.0.tar.gz`

Anyways, one of those two things work. 

That's it.

In Python:
```
import spacy
nlp = spacy.load("en_core_web_sm")
```

In R:
```
library(spacyr)
spacy_initialize(condaenv = "r-NLP", model = "en_core_web_sm")
```