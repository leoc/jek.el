(ert-deftest read-layout-form ()
  (noflet ((insert-file-contents (filename) nil)
         (buffer-string () ""
                        "(:div :class \"content\" \"Some text for that div.\")"))
    (should (equal (jekel/read-layout-forms "/some/file.html.el")
                   '(:div :class "content" "Some text for that div.")))))

(ert-deftest read-layout-multiple-forms ()
  "When a layout contains multiple s-expressions, the result should be a list of
that s-expressions."
  (noflet ((insert-file-contents (filename) nil)
         (buffer-string () ""
                        "(:div :class \"content\" \"Some text for that div.\")(:footer \"Some text for the footer.\")"))
    (should (equal (jekel/read-layout-forms "/some/file.html.el")
                   '((:div :class "content" "Some text for that div.")
                     (:footer "Some text for the footer."))))))

(ert-deftest read-layout-string-form ()
  "When a layout contains a string, the result should be that string"
  (noflet ((insert-file-contents (filename) nil)
         (buffer-string () ""
                        "\"test\""))
    (should (equal (jekel/read-layout-forms "/some/file.html.el")
                   "test"))))

(ert-deftest read-layout-multiple-string-forms ()
  "When a layout contains multiple strings, the result should be a
list of those strings."
  (noflet ((insert-file-contents (filename) nil)
         (buffer-string () ""
                        "\"test\" \"test2\""))
    (should (equal (jekel/read-layout-forms "/some/file.html.el")
                   '("test" "test2")))))

(ert-deftest read-layout-multiple-mixed-forms ()
  "When a layout contains multiple mixed expressions, the result should be a
list of those expressions."
  (noflet ((insert-file-contents (filename) nil)
         (buffer-string () ""
                        "\"test\" (:div \"content\") \"test2\" (yield)"))
    (should (equal (jekel/read-layout-forms "/some/file.html.el")
                   '("test"
                     (:div "content")
                     "test2"
                     (yield))))))
