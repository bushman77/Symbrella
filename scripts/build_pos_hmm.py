"""P-036: build apps/core/priv/pos/ewt_hmm.json from a local UD English-EWT checkout.

Usage (not part of the build):
  git clone --depth 1 https://github.com/UniversalDependencies/UD_English-EWT.git /tmp/ud_ewt
  python3 -I scripts/build_pos_hmm.py /tmp/ud_ewt apps/core/priv/pos/ewt_hmm.json

Trains a bigram HMM on train+dev, reports held-out accuracy on test, and writes
compact tables. Data derived from UD English Web Treebank, CC BY-SA 4.0
(see NOTICE.md). UPOS -> Symbrella POS family; ADP with deprel compound:prt
becomes "particle"; PUNCT/SYM/X are skipped.
"""
import sys, json, math
from collections import Counter, defaultdict

data_dir, out_path = sys.argv[1], sys.argv[2]
MAP = {"PRON": "pronoun", "DET": "determiner", "AUX": "auxiliary", "VERB": "verb", "NOUN": "noun",
       "PROPN": "proper_noun", "ADJ": "adjective", "ADV": "adverb", "ADP": "preposition", "PART": "particle",
       "CCONJ": "conjunction", "SCONJ": "conjunction", "INTJ": "interjection", "NUM": "numeral"}

def sents(path):
    sent = []
    for line in open(path, encoding="utf-8"):
        line = line.rstrip("\n")
        if not line:
            if sent: yield sent
            sent = []
            continue
        if line.startswith("#"): continue
        cols = line.split("\t")
        if "-" in cols[0] or "." in cols[0]: continue
        upos, dep = cols[3], cols[7]
        if upos in ("PUNCT", "SYM", "X"): continue
        tag = "particle" if (upos == "ADP" and dep == "compound:prt") else MAP.get(upos)
        if tag: sent.append((cols[1].lower(), tag))
    if sent: yield sent

train = list(sents(f"{data_dir}/en_ewt-ud-train.conllu")) + list(sents(f"{data_dir}/en_ewt-ud-dev.conllu"))
test = list(sents(f"{data_dir}/en_ewt-ud-test.conllu"))

trans = defaultdict(Counter); emit = defaultdict(Counter); tagc = Counter()
for s in train:
    prev = "START"
    for w, t in s:
        trans[prev][t] += 1; emit[w][t] += 1; tagc[t] += 1; prev = t
    trans[prev]["END"] += 1

tags = sorted(tagc)
def lt(a, b):
    row = trans[a]; tot = sum(row.values())
    return math.log((row[b] + 0.1) / (tot + 0.1 * (len(tags) + 1)))
T = {a: {b: lt(a, b) for b in tags + ["END"]} for a in ["START"] + tags}
E = {w: {t: math.log((c[t] + 0.01) / (tagc[t] + 0.01 * len(emit))) for t in tags} for w, c in emit.items()}
open_classes = ["noun", "verb", "adjective", "adverb", "proper_noun"]
U = {t: (math.log(0.5 / len(open_classes)) - math.log(tagc[t])) if t in open_classes else -30.0 for t in tags}
floor = {t: min(E[w][t] for w in E) for t in tags}

def em(w, t): return E[w][t] if w in E else U[t]
def viterbi(words):
    V = [{t: (T["START"][t] + em(words[0], t), [t]) for t in tags}]
    for i in range(1, len(words)):
        V.append({t: max(((V[-1][p][0] + T[p][t] + em(words[i], t), V[-1][p][1] + [t]) for p in V[-1]), key=lambda x: x[0]) for t in tags})
    return max(((s + T[t]["END"], path) for t, (s, path) in V[-1].items()), key=lambda x: x[0])[1]

ok = n = 0
for s in test:
    pred = viterbi([w for w, _ in s])
    ok += sum(p == g for p, (_, g) in zip(pred, s)); n += len(s)
print(f"held-out test accuracy: {ok}/{n} = {ok/n:.4f}")

out = {"source": "Derived from UD English Web Treebank (train+dev), CC BY-SA 4.0, https://github.com/UniversalDependencies/UD_English-EWT",
       "tags": tags,
       "transitions": {a: {b: round(v, 3) for b, v in row.items()} for a, row in T.items()},
       "emissions": {w: {t: round(v, 3) for t, v in row.items() if v > floor[t] + 1e-9} for w, row in E.items()},
       "emission_floor": {t: round(v, 3) for t, v in floor.items()},
       "unknown": {t: round(v, 3) for t, v in U.items()}}
json.dump(out, open(out_path, "w"), separators=(",", ":"))
print("wrote", out_path, "words:", len(out["emissions"]))
