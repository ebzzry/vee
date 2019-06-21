NOTES
=====


Glossary
--------

- item (n): the smallest unit of information, e.g., `("foo" "bar")`
- feed (n): raw groups of items, e.g., `(("foo" "bar") ("qux" "quux"))`
- entry (n): an instantiated item
- column (n): an instantiated feed; more accurately, an index to entries
- record (n): an entry, unit, or column
- registry (n): a particular group of entries and column in the world
- store (n): a column or a registry
- world (n): the top-level encapsulating data structure
- template (n): a source registry
- wall (n): the longest/largest column in a registry
- forge (v): to instantiate a record then add it to the registry
- field (n): a part of an entry
- selector (n): a function used to refine searches
- test (n): the predicate used to test matches between entry values
- query (n): an integer or a string
- offset (n): the distance from the start of a column to a specific entry
- unit (n): a record that is used to resize columns and set alignments
- bury (v): to make an entry hidden
- link (n): a two-way connection between records
- link (v): to create such a connection
- unbury (v): the inverse of bury
- unlink (v): to remove the links of a record
- blank (v): to the value of a entry to nil
- deregister (v): to remove an entry from a register
- void (n): a kind of registry where banished entries go.
- banish (v): to bury, send to a void, and deregister an entry


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
- [x] Define methods for auto updating of counters


### Void

- [x] Implement banish operations
- [x] Implement bury operations
- [x] Implement void registries


### Propagation

- [x] Implement match clustering
- [x] Implement column walking
- [x] Implement units and unit linking
- [ ] Implement propagation


### Interface

- [x] Reimplement/ditch the walker
- [x] Design the inheritence of the classes
- [ ] Write a Python bridge
- [ ] Write a Python API
- [ ] Handle XLSX files
- [ ] Connect to Wordnet to be able to connect “dogs” and “puppies”


### Miscellany

- [x] Implement inter-registry traversal
- [x] Handle arbitrary delimiters
- [ ] Implement automatic detection of file delimiter
- [ ] Handle arbitrary amount of columns
