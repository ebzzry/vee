(in-package #:muso)

(defvar *spacy-map*
  '("ADJ"
    "ADP"
    "ADV"
    "AUX"
    "CONJ"
    "CCONJ"
    "DET"
    "INTJ"
    "NOUN"
    "NUM"
    "PART"
    "PRON"
    "PROPN"
    "PUNCT"
    "SCONJ"
    "SYM"
    "VERB"
    "X"
    "EOL"
    "SPACE"
    )
  "List of tags that spaCy uses")

(defvar *linkup-spacy-map*
  '(("PROPER-NAME" . nil)
    ("PREPOSITION" . nil)
    ("DANGLING-PREPOSITION" . nil)
    ("TIME-PREPOSITION" . nil)
    ("CLAUSAL-PREPOSITION" . nil)
    ("AUX" . nil)
    ("BE-AUX" . nil)
    ("COMPLEMENT-AUX" . nil)
    ("HAVE-AUX" . nil)
    ("MODAL-AUX" . nil)
    ("NEGATOR" . nil)
    ("BE-VERB" . nil)
    ("HAVE-VERB" . nil)
    ("DO-VERB" . nil)
    ("GLUE" . nil)
    ("CONJUNCTION" . nil)
    ("PUNCTUATION" . nil)
    ("SENTENCE-END" . nil)
    ("QUOTE-MARK" . nil)
    ("SUBJECT-PRONOUN" . nil)
    ("OBJECT-PRONOUN" . nil)
    ("PRONOUN" . nil)
    ("POSSESSIVE-PRONOUN" . nil)
    ("DEFINITE-ARTICLE" . nil)
    ("INDEFINITE-ARTICLE" . nil)
    ("QUANTIFIER" . nil)
    ("DEMONSTRATIVE-PRONOUN" . nil)
    ("NOISE" . nil)
    ("DETERMINER" . nil)
    ("SINGULAR-DETERMINER" . nil)
    ("PLURAL-DETERMINER" . nil)
    ("|.,|" . nil)
    ("|TO|" . nil)
    ("|AND|" . nil)
    ("|OR|" . nil)
    ("|,|" . nil)
    ("|&|" . nil)
    (" .," . nil)
    ("TO" . nil)
    ("AND" . nil)
    ("OR" . nil)
    ("," . nil)
    ("&" . nil))
  "Alist of tag mapping between Linkup and spaCy.")