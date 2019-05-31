NOTES
=====


General
-------

- The goal is to create a new TSV file wherein the first column of each
  source match to each other
- This version ignores the tags


Destination format
------------------

1. the text from first source
2. the tag from first source
3. the text from third source
4. the tag from second source
5. optional flags

Essentially, there will be two column pairs.


Row format
----------

Rows are created with the text. For example:

```
Hunchback       | PROPER-NAME           | Hunchback     | NNP
```

However, column pair can be empty if an equivalent text is broken down into
more rows. For example:

```
Notre-Dame      | PROPER-MODIFIER       | Notre         | NNP
                |                       | -             | HYPH
                |                       | dame          | NN
```


Preprocesses
------------

- If the line doesn’t match the TEXT-SEPARATOR-TEXT format, remove that line.
- If the line is empty, that is it contains only a newline, remove that line.
- If the first character of a file line is the separator, remove that line.
- If the line only contains the separator, remove that line.


Notes
-----

- File processing should be done only once, and the rest will be done on
  streams.
- Create fallback row creation
- It seems that rules have to be encoded, at least with the case of spaCy,
  because it breaks down hyphenated works into multiple entries.
- Determine the main source that comparisons will be made against.
- Is the longer file going to be used the the ’wall’?
- Should the longer source be set already, then the entries from the shorter
  file will just be inserted into the location based on the longer source?
- If this is the case, then as entries from the shorter source are inserted
  to the longer source, the longer source gets popped.


Caveats
-------

Determining which source to set against may not make sense because it
make the assumption, that one has a generally longer source cluster than the
other, which is not the case. Consider the following sets:

```
Notre-Dame      | PROPER-MODIFIER       | Notre         | NNP
                |                       | -             | HYPH
                |                       | dame          | NN
also known as   | DANGLING-ADVERB       | Also          | RB
                |                       | known         | VBN
                |                       | as            | IN
```

In the first column pair, ‘Notre-Dame’ and ‘also known as’ are contiguous
items, while that is not the case with the second column pair.

Problem arises when sets are structured as such:

```
Notre-Dame      | PROPER-MODIFIER       | Notre         | NNP
                |                       | -             | HYPH
                |                       | dame          | NN
also            | DANGLING-ADVERB       | Also known as | RB
known           | DANGLING-ADVERB       |               |
as              | DANGLING-ADVERB       |               |
```

This means, that we can’t reliable use a ‘wall’ with the assumption that it
supersets the other set.

Unless, it can be clearly stated that one source is generally longer than
the other.


Notes
-----

- When a hyphen-separated text is broken down, the hyphen is part of the
  breakdown.
- When a space-separated text is broken down, the space is not part of the
  breakdown.


Notes
-----

- When a complete match is found, add entries to destination.
- When a partial match is found, check to see if the next item also matches.
- If text matches, create new entry to TSV list, then pop both columns.


Legend
------

- Item
  + The smallest unit of information, e.g., ("foo" "bar"). A raw ‘entry’

- Feed
  + Raw groups of items, e.g., (("foo" "bar") ("qux" "quux")). A raw ‘column’

- Entry
  + Instantiated ‘item’

- Column
  + Instantiated ‘feed’

- Registry
  + Contains hash tables for entries and columns
