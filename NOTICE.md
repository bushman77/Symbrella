# Notice

## Source code

Symbrella's source code is licensed under the MIT License (see `LICENSE.md`).

## Third-party data

Some data used or shipped by Symbrella is derived from openly licensed sources.
These data files are **not** covered by the MIT License; they remain under
their original licences.

- **Wiktionary** — dictionary definitions, examples and part-of-speech entries
  in the lexicon database (generated locally by the import tasks; not committed),
  and short definitions quoted in test fixtures. Source: Wiktionary contributors,
  https://en.wiktionary.org. Licence: Creative Commons Attribution-ShareAlike 4.0
  (CC BY-SA 4.0), https://creativecommons.org/licenses/by-sa/4.0/. Restructured
  for Symbrella.

- **Universal Dependencies English Web Treebank** — the part-of-speech
  transition and emission tables in `apps/core/priv/pos/ewt_hmm.json`, derived
  from the train and dev splits by `scripts/build_pos_hmm.py`. Source:
  https://github.com/UniversalDependencies/UD_English-EWT. Licence: CC BY-SA 4.0,
  https://creativecommons.org/licenses/by-sa/4.0/. Converted to bigram HMM
  probabilities and mapped to Symbrella POS families.

Adaptations of these data files, if distributed, must also be shared under
CC BY-SA 4.0 with the same attribution.
