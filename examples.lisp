;;;; examples.lisp

(in-package #:honeycomb/core)

(import-csv-file "~/e/dat/csv/userdata/userdata1.csv" :extract-header t)
(import-csv-file "~/e/dat/csv/userdata/userdata2.csv" :extract-header t)

(filter-csv-file "~/e/dat/csv/gtd/bbc_100b.csv" "~/e/test.csv" '("sourcetype" "sourcecity"))
(filter-csv-file "~/e/dat/csv/gtd/bbc_100.csv" "~/e/test.csv" '("body!"))

(import-csv-file "~/e/dat/csv/gtd/a-1.csv" :extract-header t :volume-name "bbc" :registry-name "gtd")
(import-csv-file "~/e/dat/csv/gtd/a-2.csv" :extract-header t :volume-name "bbc")

(volume-convert-cells (search-volume "bbc") "body" :transform nil)
(bind-volumes-mutually (search-volume "volume625") (search-volume "volume626") '(0))

(import-flat-file "~/e/dat/txt/test-1.txt" :volume-name "test-1" :registry-name "test-1")
(import-flat-text "The quick brown fox jumps over the lazy dog." :volume-name "text-1" :registry-name "text-1")
