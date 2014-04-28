(ert-deftest recursive-project-files ()
  "Test retrieving project files."
  (fakir-fake-file
   (list
    (fakir-file
     :filename "orgfile.org"
     :directory "/home/fake/blog"
     :mtime "Sun, Apr 20 2014 11:14:21 GMT")
    (fakir-file
     :filename "2014-04-20-some-post.org"
     :directory "/home/fake/blog/_posts"
     :mtime "Sun, Apr 20 2014 11:16:02 GMT"))
   (should (equal (jekel--project-files "/home/fake/blog")
                  '(("/home/fake/blog/orgfile.org" (21331 44045))
                    ("/home/fake/blog/_posts/2014-04-20-some-post.org" (21331 44146)))))))

(ert-deftest org-layout-dependency ()
  "Test getting file dependencies for org-file with layout."
    (fakir-fake-file
     (list
      (fakir-file
       :filename "special.html.el"
       :content "(markup-html5 (:body (:h1 \"Title\") (yield)))"
       :directory "/home/fake/blog/_layouts")
      (fakir-file
       :filename "2014-04-20-some-post.org"
       :content "#+TITLE: Some Post\r\n#+LAYOUT: special\ntext"
       :directory "/home/fake/blog/_posts"))
     (let ((base-directory "/home/fake/blog/"))
       (should (equal (jekel--file-dependencies "/home/fake/blog/_posts/2014-04-20-some-post.org")
                      '("_layouts/special.html.el"))))))
