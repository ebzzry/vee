NOTES
=====


Glossary
--------

- item (n): the smallest unit of information, e.g., `("foo" "bar")`
- feed (n): raw groups of items, e.g., `(("foo" "bar") ("qux" "quux"))`
- entry (n): an instantiated item
- column (n): an instantiated feed; more accurately, an index to entries
- record (n): either an entry or a column
- registry (n): a particular group of entries and column in the world
- world (n): the top-level encapsulating data structure
- template (n): a source registry
- wall (n): the longest/largest column in a registry
- forge (v): to instantiate a record then add it to the registry
- field (n): a part of an entry
- selectors (n): a group of functions to narrow down searches
- test (n): the predicate used to test matches between entry values
- query (n): either an integer or a string
- offset (n): the distance from the start of a column to a specific entry
- block (n): an entry that is designed to make alignments
- bury (v): make an entry hidden by making the PREV and NEXT link to each other
  bypassing the current one
- unbury (v): the inverse of bury
- unlink (v): to remove the PREV and NEXT links of an entry but keep it
  registered
- blank (v): to the value of a entry to nil
- deregister (v): to remove an entry from a register
- void (n): a kind of registry where banished entries go.
- banish (v): to bury, send to a void, and deregister an entry
- store (n): a column or a registry


TODO
----


### Model

- [x] Design the registry system
- [x] Implement light traversal
- [x] Write feed importation
- [x] Write displayer and locaters
- [x] Implement registry wall copying
- [x] Generalize spawners
- [x] Factor out counter generators
- [x] Define methods for auto-updates of counters


### Void

- [x] Implement banishment


### Propagation

- [ ] Implement propagation
- [x] Implement clustering of matches


### Interface

- [ ] Reimplement/ditch the walker
- [ ] Design the inheritence of the classes
- [ ] Write a Python bridge
- [ ] Write a Python API
- [ ] Handle XLSX files
- [ ] Connect to Wordnet to be able to connect “dogs” and “puppies”


### Miscellany

- [ ] Handle arbitrary amount of columns
- [ ] Handle arbitrary delimiters
- [ ] Implement inter-registry traversal
