(in-package #:3b-sourcemap)

(defparameter *base64* "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")


(defun vlq (x stream)
  (flet ((base64 (x) (aref *base64* x)))
    (write-char (base64 (logior (ash (ldb (byte 4 0) (abs x)) 1)
                                (if (minusp x) 1 0)
                                (if (>= (abs x) (expt 2 4)) #b100000 0)))
                stream)
    (loop for rest = (ash (abs x) -4) then (ash rest -5)
          for bits = (ldb (byte 5 0) rest)
          for more = (if (>= rest (expt 2 5)) #b100000 0)
          while (plusp rest)
          do (write-char (base64 (logior bits more)) stream))))


(assert (equal
         (loop for i in '(0 1 2 5 15 16 17 31 32 33 63 64 65 123 1234 123456 123456789
                          -1 -2 -5 -15 -16 -17 -31 -32 -33 -63 -64 -65 -123 -1234 -123456 -123456789)
               collect (with-output-to-string (s) (vlq i s)))
         '("A" "C" "E" "K" "e" "gB" "iB" "+B" "gC" "iC" "+D" "gE" "iE" "2H" "ktC" "gkxH" "qxmvrH" "D" "F" "L" "f" "hB" "jB" "/B" "hC" "jC" "/D" "hE" "jE" "3H" "ltC" "hkxH" "rxmvrH")))


(defclass segment ()
  ((offset :initarg :offset :accessor offset)
   ;; file can be NIL to indicate no source file
   (file :initform nil :initarg :file :accessor file)
   (line :initarg :line :accessor line) ; required if file is set
   (column :initarg :column :accessor column) ; required if file is set
   ;; name only usable when file is set
   (name :initarg :name :initform nil :accessor name))) ; optional


#++
(defclass mapping-line ()
  ((segments :initform (make-array '(0) :adjustable t :fill-pointer 0)
             :reader segments
             )))

(defclass sourcemap ()
  ((version :initform 3 :reader version)
   (file :initarg :file :initform "" :accessor file)
   (root :initform "" :initarg :root :accessor root)
   ;; relative filenames for now
   (sources :initform (make-array '(0) :adjustable t :fill-pointer 0)
            :reader sources)
   (%sources :initform (make-hash-table :test 'equal) :reader %sources)
   ;; todo: sources-content
   (names :initform (make-array '(0) :adjustable t :fill-pointer 0)
          :reader names)
   (%names :initform (make-hash-table :test 'equal) :reader %names)
   (mapping-lines :initform (make-array '(0) :adjustable t :fill-pointer 0)
                  :reader mapping-lines)))


(defmethod add-file ((sourcemap sourcemap) filename)
  (when filename
    (or (gethash filename (%sources sourcemap))
       (prog1
           (setf (gethash filename (%sources sourcemap)) filename)
         (vector-push-extend filename (sources sourcemap))))))

(defmethod add-name ((sourcemap sourcemap) name)
  (when name
    (or (gethash name (%names sourcemap))
        (prog1
            (setf (gethash name (%names sourcemap)) name)
          (vector-push-extend name (names sourcemap))))))

(defmethod add-segment ((sourcemap sourcemap)
                        line column source-filename source-line source-column
                        &key name)
  ;(format t "add segment @ ~s,~s~%" line column)
  (let ((lines (mapping-lines sourcemap)))
    (when (>= line (length lines))
      #++(format t "extend, l=~s, line = ~s, floor=~s~%" line (length lines)
              (floor (* 1.5 (length lines))))
      (adjust-array lines (max (1+ line) (floor (* 1.5 (length lines))))
                    :initial-element nil))
    #++(format t "fp ~s -> ~s~%" (fill-pointer lines)
            (max line (fill-pointer lines)))
    (setf (fill-pointer lines) (max (1+ line) (fill-pointer lines)))
    (unless (aref lines line)
      #++(format t "new array~%")
      (setf (aref lines line)
            (make-array '(0) :adjustable t :fill-pointer 0)))
    (vector-push-extend
     (make-instance 'segment
                    :offset column
                    :file (add-file sourcemap source-filename)
                    :line source-line
                    :column source-column
                    :name (add-name sourcemap name))
     (aref lines line))))


(defmethod write-mappings-to-stream ((sourcemap sourcemap) stream)
  (let ((files (make-hash-table :test 'equal))
        (names (make-hash-table :test 'equal)))
    (loop for name across (remove-duplicates (names sourcemap) :test 'string=)
          for i from 0
          do (setf (gethash name names) i))
    (loop for file across (remove-duplicates (sources sourcemap) :test 'string=)
          for i from 0
          do (setf (gethash file files) i))
   (loop with source-line = 0
         with source-column = 0
         with file = 0
         with name = 0
         for line across (mapping-lines sourcemap)
         for offset = 0
         for |;| = nil then (write-char #\; stream)
         do (flet ((@offset (x)
                     (prog1 (- x offset) (setf offset x)))
                   (@file (x)
                     (prog1 (- x file) (setf file x)))
                   (@name (x)
                     (prog1 (- x name) (setf name x)))
                   (@line (x)
                     (prog1 (- x source-line) (setf source-line x)))
                   (@col (x)
                     (prog1 (- x source-column) (setf source-column x))))
              (when line
                (loop for segment across line
                     for |,| = nil then (write-char #\, stream)
                     do (vlq (@offset (offset segment)) stream)
                        (when (file segment)
                          (vlq (@file (gethash (file segment) files)) stream)
                          (vlq (@line (line segment)) stream)
                          (vlq (@col (column segment)) stream)
                          (when (name segment)
                            (vlq (@name (gethash (name segment) names)) stream)))))))))


(defmethod write-source-map-to-stream ((sourcemap sourcemap) stream
                                       &key (anti-xssi t))
  (when anti-xssi (format stream ")]}~%"))
  (format stream "{~%")
  (format stream "  \"version\": ~a,~%" (version sourcemap))
  (format stream "  \"file\": \"~a\",~%" (file sourcemap))
  (format stream "~@[  \"sourceRoot\": \"~a\",~%~]" (root sourcemap))
  (format stream "  \"sources\": [~{\"~a\"~^, ~}],~%" (coerce (sources sourcemap) 'list))
  #++
  (format stream "~@[  sourceContent: [~{~:[null~;~:*'~a'~]~^,~}]~%~]"
          (coerce (source-content sourcemap) 'list))
  (format stream "  \"names\": [~{\"~a\"~^, ~}],~%" (coerce (names sourcemap) 'list))
  (format stream "  \"mappings\": \"")
  (write-mappings-to-stream sourcemap stream)
  (format stream "\"~%}"))

#++
(string=
 (let ((s (make-instance 'sourcemap)))
   (add-segment s 0 0 "foo.js" 16 1)
   (add-segment s 0 9 "foo.js" 16 9)
   (add-segment s 0 10 "foo.js" 16 11 :name "are")
   (with-output-to-string (ss) (write-mappings-to-stream s ss)))
 "AAgBC,SAAQ,CAAEA")

#++
(string=
 (let ((s (make-instance 'sourcemap)))
   (add-segment s 10 35 "foo.js" 33 2 :name "christopher")
   (with-output-to-string (ss) (write-mappings-to-stream s ss)))
 ";;;;;;;;;;mCAiCEA")



(assert
  ;; from https://github.com/mozilla/source-map/blob/master/test/source-map/test-source-map-generator.js
 (string=
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
       (with-output-to-string (ss) (write-source-map-to-stream s ss :anti-xssi nil)))


     "{
  \"version\": 3,
  \"file\": \"min.js\",
  \"sourceRoot\": \"/the/root\",
  \"sources\": [\"one.js\", \"two.js\"],
  \"names\": [\"bar\", \"baz\", \"n\"],
  \"mappings\": \"CAAC,IAAI,IAAM,SAAUA,GAClB,OAAOC,IAAID;CCDb,IAAI,IAAM,SAAUE,GAClB,OAAOA\"
}"))

