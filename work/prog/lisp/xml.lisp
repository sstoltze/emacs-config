(ql:quickload :xmls)

(with-open-file (f "test.xml")
;                (format t (read-line f))
                (let ((xml (xmls:parse f)))
                  (format t xml)))
