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
