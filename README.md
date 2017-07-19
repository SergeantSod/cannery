-[ ] Rename to cannery
-[ ] Add example .yml
-[ ] Support Co-Product in YamlReads type class
    * in case of Left, fall back to the Reads for the right side of the co-product, use flatmap
    * for CNil we have failed, since none of the cases matched
-[ ] Rename Yaml-related type classes
-[ ] Write Readme with disclaimer
-[ ] Push to Github
-[ ] Add tests for the interesting bits.
    - [ ] YAML-related type class derivation
    - [ ] Consider extracting it into separate project
