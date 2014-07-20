Writer for [javascript source maps](https://docs.google.com/document/d/1U1RGAehQwRypUTovF1KRlpiOFze0b-_2gc6fAH0KY0k).


Example usage (API subject to change):
 (based on one of the tests at https://github.com/mozilla/source-map/blob/master/test/source-map/test-source-map-generator.js)

```lisp
(let ((s (make-instance 'sourcemap :file "min.js" :root "/the/root")))
  (add-segment s 0 1 "one.js" 0 1)
  (add-segment s 0 5 "one.js" 0 5)
  (add-segment s 0 9 "one.js" 0 11)
  (add-segment s 0 18 "one.js" 0 21 :name "bar")
  (add-segment s 0 21 "one.js" 1 3)
  (add-segment s 0 28 "one.js" 1 10 :name "baz")
  (add-segment s 0 32 "one.js" 1 14 :name "bar")
  (add-segment s 1 1 "two.js" 0 1)
  (add-segment s 1 5 "two.js" 0 5)
  (add-segment s 1 9 "two.js" 0 11)
  (add-segment s 1 18 "two.js" 0 21 :name "n")
  (add-segment s 1 21 "two.js" 1 3)
  (add-segment s 1 28 "two.js" 1 10 :name "n")
  (with-output-to-string (stream)
    (write-source-map-to-stream s stream
                                ;; spec says to prepend ")]}" to file, but
                                ;; firefox doesn't seem to support that yet
                                ;; so turn it off for now...
                                :anti-xssi nil)))
```

output:

```json
{
  "version": 3,
  "file": "min.js",
  "sourceRoot": "/the/root",
  "sources": ["one.js", "two.js"],
  "names": ["bar", "baz", "n"],
  "mappings": "CAAC,IAAI,IAAM,SAAUA,GAClB,OAAOC,IAAID;CCDb,IAAI,IAAM,SAAUE,GAClB,OAAOA"
}
```