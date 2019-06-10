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
- wall copy (v): to create a copy of a template to a new registry, wherein all
  columns in the new registry have the same lengths as the wall from template
- forge (v): to instantiate a record then add it to the registry
- field (n): a part of an entry


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
- [x] Define methods fro auto-updates of counters


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
